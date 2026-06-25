package nl.rivm.screenit.service.mamma.afspraakzoeken;

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
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaDossier;

public interface MammaAfspraakOptieAlgoritme
{
	List<MammaAfspraakOptie> getAfspraakOpties(MammaDossier dossier, MammaStandplaatsPeriodeMetZoekbereik standplaatsPeriodeMetZoekbereik, boolean extraOpties,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen);

	List<MammaAfspraakOptie> getAfspraakOptieBulkVerzetten(MammaDossier dossier, MammaStandplaatsPeriodeMetZoekbereik standplaatsPeriodeMetZoekbereik,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen);

	MammaAfspraakOptie getAfspraakOptieUitnodiging(MammaDossier dossier, List<MammaStandplaatsPeriodeMetZoekbereik> standplaatsPeriodenMetZoekbereik,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
		throws MammaOnvoldoendeVrijeCapaciteitException;
}
