package nl.rivm.screenit.util.colon;

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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.beans.support.PropertyComparator;

public class ColonScreeningRondeUtil
{

	private ColonScreeningRondeUtil()
	{

	}

	public static ColonFitRegistratie getEersteGunstigeFitRegistratie(ColonScreeningRonde ronde)
	{
		ColonFitRegistratie eersteRegistratieMetGunstigeUitslag = null;
		for (var fitRegistratie : ronde.getFitRegistraties())
		{
			if (ColonFitRegistratieUtil.isGunstig(fitRegistratie)
				&& ColonFitRegistratieStatus.UITGEVOERD.equals(fitRegistratie.getStatus()) && ColonFitType.GOLD.equals(fitRegistratie.getType())
				&& (eersteRegistratieMetGunstigeUitslag == null || DateUtil.compareAfter(eersteRegistratieMetGunstigeUitslag.getStatusDatum(), fitRegistratie.getStatusDatum())))
			{
				eersteRegistratieMetGunstigeUitslag = fitRegistratie;
			}
		}
		return eersteRegistratieMetGunstigeUitslag;
	}

	public static ColonFitRegistratie getEersteOngunstigeFitRegistratie(ColonScreeningRonde ronde)
	{
		ColonFitRegistratie eersteOngunstigeRegistratie = null;
		for (var fitRegistratie : ronde.getFitRegistraties())
		{
			if (ColonFitRegistratieUtil.isOngunstig(fitRegistratie)
				&& (eersteOngunstigeRegistratie == null || DateUtil.toLocalDateTime(eersteOngunstigeRegistratie.getStatusDatum())
				.isAfter(DateUtil.toLocalDateTime(fitRegistratie.getStatusDatum()))))
			{
				eersteOngunstigeRegistratie = fitRegistratie;
			}
		}
		return eersteOngunstigeRegistratie;
	}

	public static boolean zijnErActieveFitRegistraties(ColonScreeningRonde ronde)
	{
		var activeFitRegistraties = false;
		for (var registratie : ronde.getFitRegistraties())
		{
			if (ColonFitRegistratieStatus.ACTIEF.equals(registratie.getStatus()) && ColonFitType.GOLD.equals(registratie.getType()))
			{
				activeFitRegistraties = true;
				break;
			}
		}
		return activeFitRegistraties;
	}

	public static boolean zijnErOngunstigeFitRegistraties(ColonScreeningRonde ronde)
	{
		return getEersteOngunstigeFitRegistratie(ronde) != null;
	}

	public static boolean heeftUitslagBrief(ColonScreeningRonde ronde)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_UITSLAG_BRIEVEN.contains(brief.getBriefType()));
	}

	public static boolean heeftBuitenDoelgroepBrief(ColonScreeningRonde ronde)
	{
		return ronde.getBrieven().stream().anyMatch(brief -> BriefType.COLON_UITSLAGBRIEF_ONBEOORDEELBAAR_BUITEN_DOELGROEP.equals(brief.getBriefType())
			|| BriefType.COLON_UITSLAGBRIEF_ONGUNSTIGE_BUITEN_DOELGROEP.equals(brief.getBriefType()));
	}

	public static boolean heeftAfgerondeVerslag(ColonScreeningRonde screeningRonde, VerslagType... types)
	{
		return screeningRonde.getVerslagen().stream()
			.anyMatch(v -> (types == null || types.length == 0 || Arrays.asList(types).contains(v.getType()))
				&& v.getStatus() == VerslagStatus.AFGEROND);
	}

	public static boolean magUitnodigingMetFitMaken(ColonDossier dossier, int aantalRondesUitnodigingsbriefZonderFit)
	{
		List<ColonScreeningRonde> rondes = new ArrayList<>(dossier.getScreeningRondes());
		Collections.sort(rondes, new PropertyComparator<>("creatieDatum", false, false));
		if (rondes.size() < aantalRondesUitnodigingsbriefZonderFit)
		{
			return true;
		}
		for (var ronde : rondes)
		{
			for (var fitRegistratie : ronde.getFitRegistraties())
			{
				if (fitRegistratie.getAnalyseDatum() != null || ColonFitRegistratieStatus.VERWIJDERD.equals(fitRegistratie.getStatus()))
				{
					return true;
				}
			}
			if (ronde.getLaatsteAfmelding() != null)
			{
				return true;
			}
			aantalRondesUitnodigingsbriefZonderFit--;
			if (aantalRondesUitnodigingsbriefZonderFit == 0)
			{
				return false;
			}
		}
		return true;
	}

	public static boolean isLaatsteScreeningRondeNietVerlopen(ColonScreeningRonde laatsteScreeningRonde)
	{
		return laatsteScreeningRonde != null && !(ScreeningRondeStatus.AFGEROND.equals(laatsteScreeningRonde.getStatus())
			&& Constants.RONDE_AFROND_REDEN_BUITEN_DOELGROEP.equals(laatsteScreeningRonde.getAfgerondReden()));
	}

	public static boolean isLaatsteScreeningRondGeldigEnAangemeld(ColonScreeningRonde laatsteScreeningRonde)
	{
		return isLaatsteScreeningRondeNietVerlopen(laatsteScreeningRonde) && Boolean.TRUE.equals(laatsteScreeningRonde.getAangemeld());
	}
}
