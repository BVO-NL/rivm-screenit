package nl.rivm.screenit.batch.score;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.model.ClientAfspraak;

import ai.timefold.solver.core.api.score.HardSoftScore;
import ai.timefold.solver.core.api.score.stream.Constraint;
import ai.timefold.solver.core.api.score.stream.ConstraintFactory;
import ai.timefold.solver.core.api.score.stream.ConstraintProvider;
import ai.timefold.solver.core.api.score.stream.Joiners;

public class IntakePlanningConstraintProvider implements ConstraintProvider
{
	@Override
	public Constraint[] defineConstraints(ConstraintFactory factory)
	{
		return new Constraint[] {
			eenAfspraakPerSlot(factory),
			wachttijd(factory),
			afstand(factory),
		};
	}

	protected Constraint eenAfspraakPerSlot(ConstraintFactory factory)
	{
		return factory.forEachUniquePair(ClientAfspraak.class,
				Joiners.equal(ClientAfspraak::getVrijSlot))
			.penalize(HardSoftScore.ONE_HARD)
			.asConstraint("eenAfspraakPerSlot");
	}

	protected Constraint wachttijd(ConstraintFactory factory)
	{
		return factory.forEach(ClientAfspraak.class)
			.penalize(HardSoftScore.ONE_SOFT, ClientAfspraak::getWachttijd)
			.asConstraint("wachttijd");
	}

	protected Constraint afstand(ConstraintFactory factory)
	{
		return factory.forEach(ClientAfspraak.class)
			.penalize(HardSoftScore.ONE_SOFT, ClientAfspraak::getAfstand)
			.asConstraint("afstand");
	}
}
