package nl.rivm.screenit.service.mamma.afspraakzoeken.impl;

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
import java.time.LocalDate;

import lombok.Getter;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;

@Getter
class MammaAfspraakOptieZoekContext
{
	private final MammaDossier dossier;

	private final BigDecimal factor;

	private final boolean dubbeleTijd;

	private final boolean enkeleMammograaf;

	private final BigDecimal benodigdeCapaciteit;

	private final BigDecimal benodigdeCapaciteitPerMindervalideAfspraak;

	private final int benodigdeMinutenVoorMindervalideAfspraak;

	private final Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen;

	private final LocalDate vrijgevenMindervalideReserveringenTotEnMetDatum;

	MammaAfspraakOptieZoekContext(MammaDossier dossier, BigDecimal factor, BigDecimal voorlopigeOpkomstkans, MammaScreeningsEenheid screeningsEenheid,
		ScreeningOrganisatie screeningOrganisatie, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, LocalDate vrijgevenMindervalideReserveringenTotEnMetDatum)
	{
		this.dossier = dossier;
		this.factor = factor;
		this.capaciteitVolledigBenutTotEnMetAantalWerkdagen = capaciteitVolledigBenutTotEnMetAantalWerkdagen;
		dubbeleTijd = dossier.getDoelgroep().equals(MammaDoelgroep.DUBBELE_TIJD) || dossier.getTehuis() != null;
		enkeleMammograaf = MammaPlanningUtil.isEnkeleMammograaf(screeningsEenheid);
		benodigdeCapaciteit = MammaPlanningUtil.bepaalBenodigdeCapaciteitVoorNieuweAfspraakOptie(factor, voorlopigeOpkomstkans);
		benodigdeCapaciteitPerMindervalideAfspraak = screeningOrganisatie.getFactorMinderValideBk();
		benodigdeMinutenVoorMindervalideAfspraak = MammaPlanningUtil.benodigdeMinutenVoorMindervalideAfspraak(benodigdeCapaciteitPerMindervalideAfspraak);
		this.vrijgevenMindervalideReserveringenTotEnMetDatum = vrijgevenMindervalideReserveringenTotEnMetDatum;
	}
}
