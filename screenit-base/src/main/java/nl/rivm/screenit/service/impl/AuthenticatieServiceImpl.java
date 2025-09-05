package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.repository.algemeen.MedewerkerRepository;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.util.CodeGenerator;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
@Service
public class AuthenticatieServiceImpl implements AuthenticatieService
{

	@Autowired
	private MedewerkerRepository medewerkerRepository;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private MailService mailService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private OrganisatieService organisatieService;

	@Autowired
	@Qualifier(value = "applicationUrl")
	private String applicationUrl;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public Medewerker requestNewPassword(String gebruikersnaam, String emailextra)
	{
		Medewerker medewerker = null;
		try
		{
			if (StringUtils.isNotBlank(gebruikersnaam))
			{
				medewerker = medewerkerRepository.findByGebruikersnaam(gebruikersnaam).orElse(null);
			}
			else if (StringUtils.isNotBlank(emailextra))
			{
				medewerker = medewerkerRepository.findByEmailextraAndActief(emailextra, true).orElse(null);
			}

			if (medewerker != null && medewerker.getEmailextra() != null && medewerker.getInlogMethode() != InlogMethode.UZIPAS)
			{
				var codeB = CodeGenerator.genereerCode(4, 5);
				medewerker.setDatumWachtwoordAanvraag(currentDateSupplier.getDate());
				medewerker.setWachtwoordChangeCode(codeB);
				var url = applicationUrl;
				if (!url.endsWith("/"))
				{
					url += "/";
				}
				var content = simplePreferenceService.getString(PreferenceKey.WACHTWOORDEMAIL.name(), "{link}");
				var link = "<a href=\"" + url + "passwordchange?code=" + codeB + "&user=" + medewerker.getGebruikersnaam() + "\">Wachtwoord</a>";

				var aanhef = "";
				if (medewerker.getAanhef() != null)
				{
					aanhef = " " + medewerker.getAanhef().getNaam();
				}

				var titel = "";
				if (medewerker.getTitel() != null)
				{
					titel = " " + medewerker.getTitel().getNaam();
				}

				var achternaam = "";
				if (StringUtils.isNotBlank(medewerker.getAchternaam()))
				{
					achternaam = " " + medewerker.getAchternaam();
				}

				var tussenvoegsel = "";
				if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
				{
					tussenvoegsel = " " + medewerker.getTussenvoegsel();
				}

				var voorletters = "";
				if (StringUtils.isNotBlank(medewerker.getVoorletters()))
				{
					voorletters = " " + medewerker.getVoorletters();
				}

				content = content.replaceAll("\\{link\\}", link);
				content = content.replaceAll("\\{aanhef\\}", aanhef);
				content = content.replaceAll("\\{titel\\}", titel);
				content = content.replaceAll("\\{achternaam\\}", achternaam);
				content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				content = content.replaceAll("\\{voorletters\\}", voorletters);
				content = content.replaceAll("\\{gebruikersnaam\\}", medewerker.getGebruikersnaam());

				var emailAdres = medewerker.getEmailextra();

				mailService.queueMailAanProfessional(emailAdres, simplePreferenceService.getString(PreferenceKey.WACHTWOORDEMAILSUBJECT.name(), "Geen onderwerp"), content);
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return medewerker;
	}

	@Override
	public Map<Medewerker, Boolean> accountGeblokkeerd(Medewerker medewerker)
	{
		var gebruikerGelukt = new HashMap<Medewerker, Boolean>();
		try
		{

			if (medewerker != null && medewerker.getEmailextra() != null)
			{
				var content = simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAIL.name(), "U account is geblokkeerd door een beheerder.");

				var aanhef = "";
				if (medewerker.getAanhef() != null)
				{
					aanhef = " " + medewerker.getAanhef().getNaam();
				}

				var titel = "";
				if (medewerker.getTitel() != null)
				{
					titel = " " + medewerker.getTitel().getNaam();
				}

				var achternaam = "";
				if (StringUtils.isNotBlank(medewerker.getAchternaam()))
				{
					achternaam = " " + medewerker.getAchternaam();
				}

				var tussenvoegsel = "";
				if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
				{
					tussenvoegsel = " " + medewerker.getTussenvoegsel();
				}

				var voorletters = "";
				if (StringUtils.isNotBlank(medewerker.getVoorletters()))
				{
					voorletters = " " + medewerker.getVoorletters();
				}

				content = content.replaceAll("\\{aanhef\\}", aanhef);
				content = content.replaceAll("\\{titel\\}", titel);
				content = content.replaceAll("\\{achternaam\\}", achternaam);
				content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
				content = content.replaceAll("\\{voorletters\\}", voorletters);
				content = content.replaceAll("\\{gebruikersnaam\\}", medewerker.getGebruikersnaam());

				var emailAdres = medewerker.getEmailwerk();
				if (StringUtils.isNotBlank(medewerker.getEmailextra()))
				{
					emailAdres = medewerker.getEmailextra();
				}

				mailService.queueMailAanProfessional(emailAdres, simplePreferenceService.getString(PreferenceKey.GEBLOKKEERDEMAILSUBJECT.name(), "Account geblokkeerd"), content);
				gebruikerGelukt.put(medewerker, true);
			}
			else
			{
				gebruikerGelukt.put(medewerker, false);
			}
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage(), e);
		}
		return gebruikerGelukt;
	}

	@Override
	public void sendUziEmail(Medewerker medewerker)
	{
		if (medewerker.getEmailextra() != null)
		{
			var content = simplePreferenceService.getString(PreferenceKey.UZIEMAIL.name(), "Het Uzinummer is toegevoegd.");

			var aanhef = "";
			if (medewerker.getAanhef() != null)
			{
				aanhef = " " + medewerker.getAanhef().getNaam();
			}

			var titel = "";
			if (medewerker.getTitel() != null)
			{
				titel = " " + medewerker.getTitel().getNaam();
			}

			var achternaam = "";
			if (StringUtils.isNotBlank(medewerker.getAchternaam()))
			{
				achternaam = " " + medewerker.getAchternaam();
			}

			var tussenvoegsel = "";
			if (StringUtils.isNotBlank(medewerker.getTussenvoegsel()))
			{
				tussenvoegsel = " " + medewerker.getTussenvoegsel();
			}

			var voorletters = "";
			if (StringUtils.isNotBlank(medewerker.getVoorletters()))
			{
				voorletters = " " + medewerker.getVoorletters();
			}

			content = content.replaceAll("\\{uzinummer\\}", medewerker.getUzinummer());
			content = content.replaceAll("\\{aanhef\\}", aanhef);
			content = content.replaceAll("\\{titel\\}", titel);
			content = content.replaceAll("\\{achternaam\\}", achternaam);
			content = content.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
			content = content.replaceAll("\\{voorletters\\}", voorletters);
			content = content.replaceAll("\\{gebruikersnaam\\}", medewerker.getGebruikersnaam());

			var emailAdres = medewerker.getEmailwerk();
			if (StringUtils.isNotBlank(medewerker.getEmailextra()))
			{
				emailAdres = medewerker.getEmailextra();
			}

			mailService.queueMailAanProfessional(emailAdres, simplePreferenceService.getString(PreferenceKey.UZIEMAILSUBJECT.name(), "Geen onderwerp"), content);
		}

	}

	@Override
	public List<OrganisatieMedewerker> getActieveOrganisatieMedewerkers(Medewerker medewerker)
	{
		return organisatieService.getActieveOrganisatieMedewerkersMetRollen(medewerker);
	}

	@Override
	public Boolean isAccountLocked(Medewerker medewerker)
	{
		var foutieveAanmeldpogingenTimeout = simplePreferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
		if (foutieveAanmeldpogingenTimeout == null)
		{
			foutieveAanmeldpogingenTimeout = 30;
		}
		return medewerker != null &&
			(InlogStatus.GEBLOKKEERD.equals(medewerker.getInlogstatus()) ||
				(InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(medewerker.getInlogstatus())
					&& DateUtil.compareAfter(DateUtil.plusTijdseenheid(medewerker.getTijdLaatsteFoutieveInlog(), foutieveAanmeldpogingenTimeout, ChronoUnit.MINUTES),
					currentDateSupplier.getDate())));
	}

	@Override
	@Transactional
	public void unlockAccount(Medewerker medewerker)
	{

		medewerker.setFoutieveInlogpogingen(0);
		medewerker.setTijdLaatsteFoutieveInlog(null);
		medewerker.setInlogstatus(InlogStatus.OK);
		hibernateService.saveOrUpdate(medewerker);
	}

	@Override
	public void foutieveInlogpoging(Medewerker medewerker)
	{
		if (medewerker != null && InlogStatus.OK.equals(medewerker.getInlogstatus()))
		{
			var foutieveAanmeldpogingenTimeout = simplePreferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
			if (foutieveAanmeldpogingenTimeout == null)
			{
				foutieveAanmeldpogingenTimeout = 30;
			}
			var maxFoutieveAanmeldpogingen = simplePreferenceService.getInteger(PreferenceKey.MAXIMUM_FOUTIEVE_AANMELDPOGINGEN.name());
			if (maxFoutieveAanmeldpogingen == null)
			{
				maxFoutieveAanmeldpogingen = 3;
			}
			var foutieveInlogpogingen = medewerker.getFoutieveInlogpogingen();
			if (foutieveInlogpogingen == null)
			{
				foutieveInlogpogingen = 1;
			}
			else
			{
				foutieveInlogpogingen++;
			}
			medewerker.setFoutieveInlogpogingen(foutieveInlogpogingen);

			if (medewerker.getTijdLaatsteFoutieveInlog() == null)
			{
				medewerker.setTijdLaatsteFoutieveInlog(currentDateSupplier.getDate());
			}
			var diff = currentDateSupplier.getDate().getTime() - medewerker.getTijdLaatsteFoutieveInlog().getTime();
			var blokkeren = foutieveInlogpogingen >= maxFoutieveAanmeldpogingen && diff < 60000L * foutieveAanmeldpogingenTimeout;
			if (blokkeren)
			{
				medewerker.setInlogstatus(InlogStatus.TIJDELIJK_GEBLOKKEERD);
			}

			medewerker.setTijdLaatsteFoutieveInlog(currentDateSupplier.getDate());
			hibernateService.saveOrUpdate(medewerker);
		}
	}

}
