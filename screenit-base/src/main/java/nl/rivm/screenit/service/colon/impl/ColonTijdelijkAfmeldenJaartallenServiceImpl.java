package nl.rivm.screenit.service.colon.impl;

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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonTijdelijkAfmeldenJaartallenService;
import nl.rivm.screenit.util.ColonScreeningRondeUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ColonTijdelijkAfmeldenJaartallenServiceImpl implements ColonTijdelijkAfmeldenJaartallenService
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ColonDossierBaseService colonDossierBaseService;

	public List<Integer> bepaalMogelijkeAfmeldJaren(Client client)
	{
		return bepaalMogelijkeAfmeldJaren(client, currentDateSupplier.getLocalDate());
	}

	@Override
	public List<Integer> bepaalMogelijkeAfmeldJaren(Client client, LocalDate peildatum)
	{
		var afmeldbaarTotMetJaartallen = new ArrayList<Integer>();
		var dossier = client.getColonDossier();
		var laatsteScreeningRondeAanwezig = dossier != null && dossier.getLaatsteScreeningRonde() != null;

		if (laatsteScreeningRondeAanwezig && heeftGeenOngunstigeUitslagInLaatsteRonde(dossier))
		{
			var begindatumLaatsteScreeningRonde = DateUtil.toLocalDate(client.getColonDossier().getLaatsteScreeningRonde().getCreatieDatum());
			var geboortedatumClient = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum());
			var volgendeUitnodigingsDatum = colonDossierBaseService.getDatumVolgendeUitnodiging(dossier);

			var maximumLeeftijd = preferenceService.getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
			var leeftijdGeenUitnodigingMeer = maximumLeeftijd + 1L;

			for (int toeTeVoegenJaren = 2; toeTeVoegenJaren <= 5; toeTeVoegenJaren++)
			{
				var teKiezenJaartal = begindatumLaatsteScreeningRonde.plusYears(toeTeVoegenJaren).getYear();

				var nieuweUitnodigingNietInDitJaar = teKiezenJaartal > peildatum.getYear();
				var nieuweDatumVoorDatumDatClientTeOudIs = peildatum.plusYears(toeTeVoegenJaren).minusYears(leeftijdGeenUitnodigingMeer).isBefore(geboortedatumClient);
				var nieuweDatumNietVoorUitnodigingsdatum =
					volgendeUitnodigingsDatum == null || !begindatumLaatsteScreeningRonde.plusYears(toeTeVoegenJaren).isBefore(volgendeUitnodigingsDatum);

				if (nieuweUitnodigingNietInDitJaar && nieuweDatumVoorDatumDatClientTeOudIs && nieuweDatumNietVoorUitnodigingsdatum)
				{
					afmeldbaarTotMetJaartallen.add(teKiezenJaartal);
				}
			}
		}
		return afmeldbaarTotMetJaartallen;
	}

	private boolean heeftGeenOngunstigeUitslagInLaatsteRonde(ColonDossier dossier)
	{
		var laatsteRonde = dossier.getLaatsteScreeningRonde();
		return ColonScreeningRondeUtil.getEersteOngunstigeTest(laatsteRonde) == null;
	}
}
