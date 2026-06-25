package nl.rivm.screenit.clientportaal.services.cervix.impl;

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

import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.clientportaal.services.cervix.CervixUitnodigingService;
import nl.rivm.screenit.clientportaal.services.cervix.CervixZasService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseScreeningrondeService;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class CervixUitnodigingServiceImpl implements CervixUitnodigingService
{
	private final CervixBaseScreeningrondeService screeningrondeService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final CervixZasService zasService;

	private static final List<BriefType> GEACCEPTEERDE_BRIEF_TYPES_VOOR_HERINNERING = List.of(
		BriefType.CERVIX_ZAS_NON_RESPONDER,
		BriefType.CERVIX_LAATSTE_HERINNERING,
		BriefType.CERVIX_HERINNERING_ZAS_UITNODIGING,
		BriefType.CERVIX_HERINNERING_UITNODIGING
	);

	private static final List<BriefType> NIET_GEACCEPTEERDE_BRIEF_TYPES = List.of(BriefType.CERVIX_HERINNERING_30_JARIGEN,
		BriefType.CERVIX_HERINNERING_ZAS_HPV_POSITIEF,
		BriefType.CERVIX_HERINNERING_UITNODIGING_CONTROLEUITSTRIJKJE);

	@Override
	public CervixUitnodiging getOpenstaandeZasUitnodiging(Client client)
	{
		var dossier = client.getCervixDossier();
		return Optional.ofNullable(dossier)
			.filter(cervixDossier -> !AfmeldingUtil.isEenmaligOfDefinitiefAfgemeld(cervixDossier))
			.map(CervixDossier::getLaatsteScreeningRonde)
			.filter(this::heeftGeaccepteerdeBriefVoorHerinnering)
			.filter(this::heeftGeenBrievenDieNietGeaccepteerdZijnVoorHerinnering)
			.filter(this::herinneringIsVerGenoegInVerleden)
			.filter(this::heeftGeenOpenstaandeAanvraag)
			.filter(screeningRonde -> !isUitgesteld(screeningRonde))
			.map(CervixScreeningRonde::getLaatsteUitnodiging)
			.filter(this::isZas)
			.filter(uitnodiging -> !heeftUitslagUitUitnodiging(uitnodiging))
			.orElse(null);
	}

	private boolean heeftUitslagUitUitnodiging(CervixUitnodiging uitnodiging)
	{
		return screeningrondeService.heeftUitslagOfHeeftGehad(uitnodiging);
	}

	private boolean isZas(CervixUitnodiging uitnodiging)
	{
		return CervixMonsterUtil.isZAS(uitnodiging.getMonster());
	}

	private boolean heeftGeaccepteerdeBriefVoorHerinnering(CervixScreeningRonde ronde)
	{
		return GEACCEPTEERDE_BRIEF_TYPES_VOOR_HERINNERING.stream().anyMatch(briefType -> heeftBriefInRonde(ronde, briefType));
	}

	private boolean heeftGeenBrievenDieNietGeaccepteerdZijnVoorHerinnering(CervixScreeningRonde ronde)
	{
		return NIET_GEACCEPTEERDE_BRIEF_TYPES.stream().noneMatch(briefType -> heeftBriefInRonde(ronde, briefType));
	}

	private boolean heeftBriefInRonde(CervixScreeningRonde ronde, BriefType briefType)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> brief.getBriefType() == briefType);
	}

	private boolean herinneringIsVerGenoegInVerleden(CervixScreeningRonde ronde)
	{
		var herinnering = ronde.getBrieven().stream()
			.filter(this::isGeaccepteerdeBriefVoorHerinnering)
			.filter(BriefUtil::isVerstuurd)
			.max(Comparator.comparing(CervixBrief::getCreatieDatum, Comparator.nullsFirst(Date::compareTo)));

		if (herinnering.isEmpty())
		{
			return false;
		}

		var wekenNaAanmaakHerinnering = preferenceService.getInteger(PreferenceKey.CERVIX_WEKEN_NA_AANMAAK_HERINNERING.name());
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

	private boolean isGeaccepteerdeBriefVoorHerinnering(CervixBrief brief)
	{
		return GEACCEPTEERDE_BRIEF_TYPES_VOOR_HERINNERING.contains(brief.getBriefType());
	}

	private boolean isUitgesteld(CervixScreeningRonde screeningRonde)
	{
		return screeningRonde.getUitstel() != null
			&& screeningRonde.getUitstel().getUitstellenTotDatum().after(currentDateSupplier.getDate())
			&& screeningRonde.getUitstel().getGeannuleerdDatum() == null;
	}

	private boolean heeftGeenOpenstaandeAanvraag(CervixScreeningRonde ronde)
	{
		return !zasService.heeftOpenstaandeAanvraag(ronde);
	}
}
