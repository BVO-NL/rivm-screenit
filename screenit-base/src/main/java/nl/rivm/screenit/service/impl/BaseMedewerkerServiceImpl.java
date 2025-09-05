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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.repository.algemeen.MedewerkerRepository;
import nl.rivm.screenit.repository.algemeen.OrganisatieMedewerkerRolRepository;
import nl.rivm.screenit.service.BaseMedewerkerService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.rivm.screenit.util.DatabaseSequence;
import nl.rivm.screenit.util.SequenceGenerator;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@AllArgsConstructor
public class BaseMedewerkerServiceImpl implements BaseMedewerkerService
{
	private final SimplePreferenceService preferenceService;

	private final MailService mailService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MedewerkerRepository medewerkerRepository;

	private final OrganisatieMedewerkerRolRepository organisatieMedewerkerRolRepository;

	private final HibernateService hibernateService;

	@Override
	public Optional<Medewerker> getMedewerkerByGebruikersnaam(String gebruikersnaam)
	{
		return medewerkerRepository.findByGebruikersnaam(gebruikersnaam);
	}

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie(Medewerker medewerker)
	{
		List<ScreeningOrganisatie> screeningOrganisaties = new ArrayList<>();
		for (OrganisatieMedewerker organisatieMedewerker : medewerker.getOrganisatieMedewerkers())
		{
			if (ScreeningOrganisatie.class.isAssignableFrom(Hibernate.getClass(organisatieMedewerker.getOrganisatie())))
			{
				screeningOrganisaties.add((ScreeningOrganisatie) organisatieMedewerker.getOrganisatie());
			}
		}

		if (screeningOrganisaties.size() == 1)
		{
			return screeningOrganisaties.get(0);
		}

		return null;
	}

	@Override
	public Optional<Medewerker> getMedewerkerByUzinummer(String uzinummer)
	{
		return medewerkerRepository.findByUzinummer(uzinummer);
	}

	@Override
	public Medewerker getPatholoog(String patholoogId, Organisatie organisatie)
	{
		var pathologen = medewerkerRepository.findByPatholoogIdAndActiefTrue(patholoogId);
		Medewerker medewerker = null;
		if (pathologen != null)
		{
			if (pathologen.size() == 1)
			{
				medewerker = pathologen.get(0);
			}
			else if (pathologen.size() > 1 && organisatie != null)
			{

				boolean gebruikerNietUniek = false;
				for (Medewerker subMedewerker : pathologen)
				{
					if (subMedewerker.getOrganisatieMedewerkers() != null)
					{
						for (OrganisatieMedewerker organisatieMedewerker : subMedewerker.getOrganisatieMedewerkers())
						{
							if (Boolean.TRUE.equals(organisatieMedewerker.getActief()) && Boolean.TRUE.equals(organisatieMedewerker.getOrganisatie().getActief())
								&& organisatieMedewerker.getOrganisatie().equals(organisatie))
							{
								if (medewerker == null)
								{
									medewerker = subMedewerker;
								}
								else
								{
									gebruikerNietUniek = true;
								}
							}
						}
					}
				}
				if (gebruikerNietUniek)
				{
					medewerker = null;
				}
			}
		}
		return medewerker;
	}

	@Override
	public int getNextMedewerkercode()
	{
		return hibernateService.getHibernateSession()
			.doReturningWork(new SequenceGenerator(DatabaseSequence.MEDEWERKERCODE, hibernateService.getHibernateSession().getSessionFactory())).intValue();
	}

	@Override
	@Transactional
	public void inActiveerMedewerker(Medewerker medewerker)
	{
		medewerker.setActief(Boolean.FALSE.equals(medewerker.getActief()));
		if (medewerker.getActief().equals(Boolean.TRUE))
		{
			return;
		}

		if (medewerker.getActiefTotEnMet() == null)
		{
			medewerker.setActiefTotEnMet(currentDateSupplier.getDate());
		}
		medewerker.setInlogstatus(InlogStatus.GEBLOKKEERD);
		medewerker.getOrganisatieMedewerkers().forEach(organisatieMedewerker ->
		{
			organisatieMedewerker.setActief(Boolean.FALSE);
			inactiveerOrganisatieMedewerkersMetRol(organisatieMedewerker.getRollen());
		});
		medewerkerRepository.save(medewerker);

		if (StringUtils.isNotBlank(medewerker.getEmailextra()))
		{
			verstuurInactiverenEmail(medewerker);
		}
	}

	@Override
	@Transactional
	public void inactiveerOrganisatieMedewerkersMetRol(List<OrganisatieMedewerkerRol> medewerkersMetRol)
	{
		medewerkersMetRol.forEach(rol ->
		{
			rol.setActief(Boolean.FALSE);
			if (rol.getEindDatum() == null || rol.getEindDatum().after(currentDateSupplier.getDate()))
			{
				rol.setEindDatum(currentDateSupplier.getDate());
			}
		});
		organisatieMedewerkerRolRepository.saveAll(medewerkersMetRol);

	}

	private void verstuurInactiverenEmail(Medewerker medewerker)
	{
		var inactiverenemail = preferenceService.getString(PreferenceKey.INACTIVERENEMAIL.name(), "Beste medewerker, <br><br>"
			+ "Uw account met de gebruikersnaam '{gebruikersnaam}' is ge&iuml;nactiveerd." + " <br><br>Met vriendelijke groeten, <br>Het ScreenIT team");
		inactiverenemail = inactiverenemail.replaceAll("\\{gebruikersnaam\\}", medewerker.getGebruikersnaam());
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
		inactiverenemail = inactiverenemail.replaceAll("\\{aanhef\\}", aanhef);
		inactiverenemail = inactiverenemail.replaceAll("\\{titel\\}", titel);
		inactiverenemail = inactiverenemail.replaceAll("\\{achternaam\\}", achternaam);
		inactiverenemail = inactiverenemail.replaceAll("\\{tussenvoegsel\\}", tussenvoegsel);
		inactiverenemail = inactiverenemail.replaceAll("\\{voorletters\\}", voorletters);
		var inactiverenSubject = preferenceService.getString(PreferenceKey.INACTIVERENSUBJECT.name(), "ScreenIT - Account ge\u00EFnactiveerd");
		mailService.queueMailAanProfessional(medewerker.getEmailextra(), inactiverenSubject, inactiverenemail);
	}
}
