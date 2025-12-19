package nl.rivm.screenit.util.mamma;

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Comparator;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.util.DateUtil;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaPlanningUtil
{
	private static final int AANTAL_WERKDAGEN_TUSSEN_DATA_GRENS = 5;

	private static final BigDecimal MINIMUM_OPKOMSTKANS = new BigDecimal("0.1");

	public static boolean datumIsMeerDanVijfWerkdagenVoorDatum(LocalDate teCheckenDatum, LocalDate grensDatum)
	{
		int werkdagenTussenData = DateUtil.getDaysBetweenIgnoreWeekends(teCheckenDatum.atStartOfDay(), grensDatum.atStartOfDay(), true);
		return !teCheckenDatum.isAfter(grensDatum) && werkdagenTussenData > AANTAL_WERKDAGEN_TUSSEN_DATA_GRENS;
	}

	public static int minimumTijdvak(BigDecimal factor)
	{
		return factor.setScale(0, RoundingMode.HALF_DOWN).multiply(new BigDecimal(Constants.BK_TIJDVAK_MIN)).intValue();
	}

	public static int benodigdeMinutenVoorMindervalideAfspraak(BigDecimal factorMinderValide)
	{

		return minimumTijdvak(factorMinderValide);
	}

	public static void sorteerCapaciteitBlokOpAfspraakTijdEnZetAfspraakTot(MammaCapaciteitBlokDto capaciteitBlokDto) 
	{
		capaciteitBlokDto.getAfspraakDtos().sort(Comparator.comparing(MammaAfspraakDto::getVanaf));
		MammaAfspraakDto vorigeAfspraakDto = null;
		for (var afspraakDto : capaciteitBlokDto.getAfspraakDtos())
		{
			if (vorigeAfspraakDto != null)
			{
				vorigeAfspraakDto.setTot(afspraakDto.getVanaf().toLocalTime());
			}
			vorigeAfspraakDto = afspraakDto;
		}
		if (vorigeAfspraakDto != null)
		{
			vorigeAfspraakDto.setTot(capaciteitBlokDto.getTot());
		}
	}

	public static BigDecimal bepaalBenodigdeCapaciteitVoorNieuweAfspraakOptie(BigDecimal factor, BigDecimal voorlopigeOpkomstkans)
	{
		return factor.multiply(voorlopigeOpkomstkans.compareTo(MINIMUM_OPKOMSTKANS) >= 0 ? voorlopigeOpkomstkans : MINIMUM_OPKOMSTKANS);
	}

	public static BigDecimal getVrijeCapaciteitVanBlok(MammaCapaciteitBlokDto capaciteitBlokDto)
	{
		var totaalBenodigdeCapaciteitVoorAlleAfspraken = capaciteitBlokDto.getAfspraakDtos().stream().map(MammaAfspraakDto::getBenodigdeCapaciteit).reduce(BigDecimal.ZERO,
			BigDecimal::add);
		return capaciteitBlokDto.getBeschikbareCapaciteit().subtract(totaalBenodigdeCapaciteitVoorAlleAfspraken);
	}

	public static boolean mindervalideReserveringIsOnbezet(MammaCapaciteitBlokDto capaciteitBlokDto, LocalTime mvReserveringVanaf)
	{
		return capaciteitBlokDto.getAfspraakDtos().stream()
			.noneMatch(afspraakDto -> afspraakDto.isMindervalide() && afspraakDto.getVanaf().toLocalTime().equals(mvReserveringVanaf));
	}

	public static boolean isEnkeleMammograaf(MammaScreeningsEenheid screeningsEenheid)
	{
		return screeningsEenheid.getMammografen().size() <= 1;
	}
}
