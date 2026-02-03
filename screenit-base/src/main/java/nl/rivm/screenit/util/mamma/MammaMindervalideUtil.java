package nl.rivm.screenit.util.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaMindervalideReservering;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.util.DateUtil;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaMindervalideUtil
{
	public static boolean isMindervalideAfspraakMetReservering(MammaAfspraak afspraak, List<MammaMindervalideReservering> reserveringen)
	{
		return isMindervalideAfspraak(afspraak) && reserveringen.stream()
			.anyMatch(reservering -> heeftZelfdeVanafTijd(afspraak, reservering));
	}

	public static boolean isMindervalideReserveringOnbezet(MammaMindervalideReservering reservering, List<MammaAfspraak> afspraken)
	{
		return afspraken.stream().filter(MammaMindervalideUtil::isMindervalideAfspraak)
			.noneMatch(afspraak -> heeftZelfdeVanafTijd(afspraak, reservering));
	}

	private static boolean heeftZelfdeVanafTijd(MammaAfspraak afspraak, MammaMindervalideReservering reservering)
	{
		return DateUtil.toLocalTime(afspraak.getVanaf()).equals(reservering.getVanaf());
	}

	private static boolean isMindervalideAfspraak(MammaAfspraak afspraak)
	{
		var dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		return dossier.getDoelgroep().equals(MammaDoelgroep.MINDERVALIDE);
	}

	public static int benodigdeMinutenVoorMindervalideAfspraak(BigDecimal factorMindervalide)
	{
		return factorMindervalide.multiply(new BigDecimal(Constants.BK_TIJDVAK_MIN)).intValue();
	}

	public static boolean isMindervalideReserveringOnbezet(MammaCapaciteitBlokDto capaciteitBlokDto, LocalTime mvReserveringVanaf)
	{
		return capaciteitBlokDto.getAfspraakDtos().stream()
			.noneMatch(afspraakDto -> afspraakDto.isMindervalide() && afspraakDto.getVanaf().toLocalTime().equals(mvReserveringVanaf));
	}

	public static boolean zijnMindervalideReserveringenVrijgegeven(MammaCapaciteitBlokDto capaciteitBlokDto, LocalDate vrijgevenMindervalideReserveringenTotEnMetDatum)
	{
		return !capaciteitBlokDto.getVanaf().toLocalDate().isAfter(vrijgevenMindervalideReserveringenTotEnMetDatum);
	}
}
