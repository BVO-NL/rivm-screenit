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

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ColonFitRegistratieUtil
{
	public static final String UITSLAG_FLAG_PRO = "PRO";

	private static final Logger LOG = LoggerFactory.getLogger(ColonFitRegistratieUtil.class);

	public static boolean isOngunstig(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie != null && fitRegistratie.getGeinterpreteerdeUitslag() != null && fitRegistratie.getType().equals(ColonFitType.STUDIE))
		{
			return fitRegistratie.getGeinterpreteerdeUitslag().equals(ColonGeinterpreteerdeUitslag.ONGUNSTIG);
		}
		if (fitRegistratie == null || (fitRegistratie.getUitslag() == null && !UITSLAG_FLAG_PRO.equals(fitRegistratie.getFlag())) || fitRegistratie.getNormWaarde() == null)
		{
			LOG.trace("test, uitslag, normwaarde of geinterpreteerde uitslag (studietest) is null");
			return false;
		}

		return UITSLAG_FLAG_PRO.equals(fitRegistratie.getFlag()) || fitRegistratie.getUitslag().compareTo(fitRegistratie.getNormWaarde()) >= 0;
	}

	public static boolean isGunstig(ColonFitRegistratie fitRegistratie)
	{
		if (fitRegistratie != null && fitRegistratie.getGeinterpreteerdeUitslag() != null && fitRegistratie.getType().equals(ColonFitType.STUDIE))
		{
			return fitRegistratie.getGeinterpreteerdeUitslag().equals(ColonGeinterpreteerdeUitslag.GUNSTIG);
		}
		if (fitRegistratie == null || fitRegistratie.getUitslag() == null || fitRegistratie.getNormWaarde() == null)
		{
			LOG.trace("test, uitslag of normwaarde is null");
			return false;
		}
		return fitRegistratie.getUitslag().compareTo(fitRegistratie.getNormWaarde()) < 0;
	}

	public static String getFitRegistratieBarcode(String prefix, int length)
	{
		var randomGenerator = new Random();

		return prefix + StringUtils.leftPad(Integer.toString(randomGenerator.nextInt(99999)), length, '0');
	}

	public static String getFitRegistratieBarcode(Long value)
	{
		return Long.toHexString(value.longValue()).toUpperCase();
	}

	public static String getFitRegistratieBarcode()
	{
		return getFitRegistratieBarcode("TGD", 5);
	}

	public static ColonFitRegistratie getFitRegistratie(ColonUitnodiging uitnodiging)
	{
		ColonFitRegistratie registratie = null;
		if (uitnodiging != null)
		{
			registratie = uitnodiging.getGekoppeldeFitRegistratie();
		}
		return registratie;

	}

	public static ColonUitnodiging getUitnodiging(ColonFitRegistratie fitRegistratie)
	{
		var uitnodiging = fitRegistratie.getUitnodiging();
		if (uitnodiging == null)
		{
			uitnodiging = fitRegistratie.getUitnodigingExtra();
		}
		return uitnodiging;
	}

	public static ColonFitRegistratieStatus getActieveFitRegistratieStatusNaHeraanmelding(ColonUitnodiging uitnodiging)
	{
		if (uitnodiging != null)
		{
			var fitRegistratie = uitnodiging.getGekoppeldeFitRegistratie();

			if (fitRegistratie != null)
			{
				if (fitRegistratie.getUitslag() == null)
				{
					return ColonFitRegistratieStatus.ACTIEF;
				}
				else
				{
					return ColonFitRegistratieStatus.UITGEVOERD;
				}
			}
		}
		return null;
	}

	public static boolean isEnigeUitgevoerdeFitInZelfdeRonde(ColonFitRegistratie fitRegistratie)
	{
		return fitRegistratie.getScreeningRonde().getFitRegistraties().stream()
			.filter(t -> t.getStatus() == ColonFitRegistratieStatus.UITGEVOERD && t.getType() == ColonFitType.GOLD)
			.count()
			== 1;
	}

	public static boolean heeftMeerdereOngunstigeUitslagenInZelfdeRonde(ColonFitRegistratie fitRegistratie)
	{
		return fitRegistratie.getScreeningRonde().getFitRegistraties().stream().filter(ColonFitRegistratieUtil::isOngunstig).count() > 1;
	}

	public static boolean isLaatsteUitslagVanLaatsteRonde(ColonDossier dossier, Date statusDatumAangeklikteUitslag)
	{
		var uitgevoerdeFitRegistraties = getAlleUitgevoerdeFitRegistratiesInLaatsteRonde(dossier);
		var laatsteStatusDatum = getLaatsteStatusDatumVanFitRegistraties(uitgevoerdeFitRegistraties);
		return DateUtil.compareEquals(statusDatumAangeklikteUitslag, laatsteStatusDatum);
	}

	private static List<ColonFitRegistratie> getAlleUitgevoerdeFitRegistratiesInLaatsteRonde(ColonDossier dossier)
	{
		return dossier.getLaatsteScreeningRonde().getFitRegistraties().stream()
			.filter(fitRegistratie -> fitRegistratie != null && fitRegistratie.getStatus().equals(ColonFitRegistratieStatus.UITGEVOERD) && fitRegistratie.getType()
				.equals(ColonFitType.GOLD))
			.collect(Collectors.toList());
	}

	private static Date getLaatsteStatusDatumVanFitRegistraties(List<ColonFitRegistratie> uitgevoerdeFitRegistraties)
	{
		return uitgevoerdeFitRegistraties.stream().map(ColonFitRegistratie::getStatusDatum).filter(Objects::nonNull).max(Date::compareTo).orElse(null);
	}

	public static boolean heeftSuccesvolleAnalyse(ColonScreeningRonde ronde)
	{
		return ronde.getFitRegistraties().stream().anyMatch(ColonFitRegistratieUtil::heeftSuccesvolleAnalyse);
	}

	public static boolean heeftSuccesvolleAnalyse(ColonFitRegistratie fit)
	{
		return fit.getStatus() == ColonFitRegistratieStatus.UITGEVOERD && (ColonFitRegistratieUtil.isGunstig(fit) || ColonFitRegistratieUtil.isOngunstig(fit));
	}

	public static boolean heeftOngunstigeUitslagInLaatsteRonde(ColonDossier dossier)
	{
		return getAlleUitgevoerdeFitRegistratiesInLaatsteRonde(dossier).stream().anyMatch(ColonFitRegistratieUtil::isOngunstig);
	}

	public static boolean heeftGunstigeUitslagInLaatsteRonde(ColonDossier dossier)
	{
		return getAlleUitgevoerdeFitRegistratiesInLaatsteRonde(dossier).stream().anyMatch(ColonFitRegistratieUtil::isGunstig);
	}

	public static String getInterpretatie(ColonFitRegistratie fit, ColonFitRegistratieStatus status, boolean geenUitslag)
	{
		var interpretatie = "";
		if (ColonFitRegistratieStatus.VERWIJDERD.equals(status))
		{
			interpretatie = "Verwijderd";
		}
		else if (isGunstig(fit))
		{
			interpretatie = "Gunstig";
		}
		else if (isOngunstig(fit))
		{
			interpretatie = "Ongunstig";
		}
		else if (fit.getUitslag() != null && fit.getNormWaarde() == null)
		{
			interpretatie = "";
		}
		else if (geenUitslag)
		{
			interpretatie = "Geen uitslag";
		}
		return interpretatie;
	}
}
