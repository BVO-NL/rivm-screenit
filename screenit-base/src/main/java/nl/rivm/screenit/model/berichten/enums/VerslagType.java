package nl.rivm.screenit.model.berichten.enums;

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

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;

@Getter
@AllArgsConstructor
public enum VerslagType
{
	MDL(
		Bevolkingsonderzoek.COLON,
		VerslagGeneratie.V12,
		LogGebeurtenis.MDL_VERSLAG_VERWIJDERD,
		MdlVerslag.class),

	PA_LAB(
		Bevolkingsonderzoek.COLON,
		VerslagGeneratie.V12,
		LogGebeurtenis.PA_VERSLAG_VERWIJDERD,
		PaVerslag.class),

	CERVIX_CYTOLOGIE(
		Bevolkingsonderzoek.CERVIX,
		VerslagGeneratie.V12,
		null,
		CervixCytologieVerslag.class),

	MAMMA_PA_FOLLOW_UP(
		Bevolkingsonderzoek.MAMMA,
		VerslagGeneratie.V2,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VERWIJDERD,
		MammaFollowUpVerslag.class),

	MAMMA_PA_FOLLOW_UP_MONITOR(
		Bevolkingsonderzoek.MAMMA,
		VerslagGeneratie.V2,
		LogGebeurtenis.MAMMA_VERSLAG_FOLLOW_UP_VERWIJDERD,
		MammaFollowUpVerslag.class);

	private final Bevolkingsonderzoek bevolkingsonderzoek;

	private final VerslagGeneratie huidigeGeneratie;

	private final LogGebeurtenis verwijderdVerslagLogGebeurtenis;

	private final Class<? extends Verslag<?, ?>> clazz;

}
