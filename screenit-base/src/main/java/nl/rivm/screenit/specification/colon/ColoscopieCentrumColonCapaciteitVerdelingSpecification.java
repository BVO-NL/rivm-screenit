package nl.rivm.screenit.specification.colon;

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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling;
import nl.rivm.screenit.model.colon.ColoscopieCentrumColonCapaciteitVerdeling_;
import nl.rivm.screenit.specification.ExtendedSpecification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColoscopieCentrumColonCapaciteitVerdelingSpecification
{

	public static ExtendedSpecification<ColoscopieCentrumColonCapaciteitVerdeling> heeftIntakelocatie(ColonIntakelocatie intakelocatie)
	{
		return (r, q, cb) -> cb.equal(r.get(ColoscopieCentrumColonCapaciteitVerdeling_.intakelocatie), intakelocatie);
	}
}
