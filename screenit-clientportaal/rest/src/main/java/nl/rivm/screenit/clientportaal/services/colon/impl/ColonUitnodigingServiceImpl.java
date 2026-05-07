package nl.rivm.screenit.clientportaal.services.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Optional;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.clientportaal.services.colon.ColonUitnodigingService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonScreeningsrondeService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ColonUitnodigingServiceImpl implements ColonUitnodigingService
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ColonScreeningsrondeService screeningsrondeService;

	@Override
	public ColonUitnodiging getOpenstaandeUitnodiging(Client client)
	{
		var dossier = client.getColonDossier();
		return Optional.ofNullable(dossier)
			.filter(colonDossier -> !AfmeldingUtil.isEenmaligOfDefinitiefAfgemeld(colonDossier))
			.map(ColonDossier::getLaatsteScreeningRonde)
			.filter(this::heeftBriefInRonde)
			.filter(this::herinneringIsVerGenoegInVerleden)
			.filter(this::heeftGeenOpenstaandeAanvraag)
			.map(ColonScreeningRonde::getLaatsteUitnodiging)
			.filter(uitnodiging -> !heeftUitslagUitUitnodiging(uitnodiging))
			.orElse(null);
	}

	private boolean heeftUitslagUitUitnodiging(ColonUitnodiging uitnodiging)
	{
		return screeningsrondeService.heeftUitslag(uitnodiging, false);
	}

	private boolean heeftBriefInRonde(ColonScreeningRonde ronde)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> brief.getBriefType() == BriefType.COLON_HERINNERING);
	}

	private boolean herinneringIsVerGenoegInVerleden(ColonScreeningRonde ronde)
	{
		var herinnering = Optional.ofNullable(ronde.getLaatsteBrief())
			.filter(brief -> brief.getBriefType() == BriefType.COLON_HERINNERING)
			.filter(BriefUtil::isVerstuurd);

		if (herinnering.isEmpty())
		{
			return false;
		}

		var wekenNaAanmaakHerinnering = preferenceService.getInteger(PreferenceKey.COLON_WEKEN_NA_AANMAAK_HERINNERING.name());
		if (wekenNaAanmaakHerinnering == null || wekenNaAanmaakHerinnering == 0)
		{
			return true;
		}

		var mergedBrieven = herinnering.get().getMergedBrieven();
		if (mergedBrieven == null || mergedBrieven.getPrintDatum() == null)
		{
			return false;
		}
		var dagenTussenHerinneringEnVandaag = DateUtil.aantalDagenVerschil(mergedBrieven.getPrintDatum(), currentDateSupplier.getDate());
		var minimaleDagenTussenHerinneringEnVandaag = wekenNaAanmaakHerinnering * 7;
		return dagenTussenHerinneringEnVandaag >= minimaleDagenTussenHerinneringEnVandaag;
	}

	private boolean heeftGeenOpenstaandeAanvraag(ColonScreeningRonde ronde)
	{
		var laatsteUitnodiging = ronde.getLaatsteUitnodiging();
		return laatsteUitnodiging == null || laatsteUitnodiging.getGekoppeldeFitRegistratie() != null;
	}
}
