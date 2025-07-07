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

import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.repository.algemeen.GebruikerRepository;
import nl.rivm.screenit.repository.algemeen.InstellingGebruikerRolRepository;
import nl.rivm.screenit.service.BaseMedewerkerService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.MailService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@AllArgsConstructor
public class BaseMedewerkerServiceImpl implements BaseMedewerkerService
{
	private final SimplePreferenceService preferenceService;

	private final MailService mailService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final GebruikerRepository medewerkerRepository;

	private final InstellingGebruikerRolRepository organisatieMedewerkerRolRepository;

	@Override
	@Transactional
	public void inActiveerMedewerker(Gebruiker medewerker)
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
		medewerker.getOrganisatieMedewerkers().forEach(instellingGebruiker ->
		{
			instellingGebruiker.setActief(Boolean.FALSE);
			inactiveerOrganisatieMedewerkersMetRol(instellingGebruiker.getRollen());
		});
		medewerkerRepository.save(medewerker);

		if (StringUtils.isNotBlank(medewerker.getEmailextra()))
		{
			verstuurInactiverenEmail(medewerker);
		}
	}

	@Override
	@Transactional
	public void inactiveerOrganisatieMedewerkersMetRol(List<InstellingGebruikerRol> medewerkersMetRol)
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

	private void verstuurInactiverenEmail(Gebruiker medewerker)
	{
		var inactiverenemail = preferenceService.getString(PreferenceKey.INACTIVERENEMAIL.name(), "Beste gebruiker, <br><br>"
			+ "U gebruiker account met de gebruikersnaam '{gebruikersnaam}' is ge&iuml;nactiveerd." + " <br><br>Met vriendelijke groeten, <br>Het ScreenIT team");
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
		var inactiverenSubject = preferenceService.getString(PreferenceKey.INACTIVERENSUBJECT.name(), "ScreenIT - Gebruiker account ge\u00EFnactiveerd");
		mailService.queueMailAanProfessional(medewerker.getEmailextra(), inactiverenSubject, inactiverenemail);
	}
}
