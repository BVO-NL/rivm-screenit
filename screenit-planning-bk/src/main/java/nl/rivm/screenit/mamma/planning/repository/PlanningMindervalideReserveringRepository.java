package nl.rivm.screenit.mamma.planning.repository;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalDate;
import java.util.List;

import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsEenheid;
import nl.rivm.screenit.mamma.planning.repository.projectie.PlanningMinderValideReserveringProjectie;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok_;
import nl.rivm.screenit.model.mamma.MammaMinderValideReservering;
import nl.rivm.screenit.model.mamma.MammaMinderValideReservering_;
import nl.rivm.screenit.repository.BaseJpaRepository;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification.heeftScreeningsEenheidId;

public interface PlanningMindervalideReserveringRepository extends BaseJpaRepository<MammaMinderValideReservering>
{
	default List<PlanningMinderValideReserveringProjectie> leesMindervalideReserveringen(PlanningScreeningsEenheid screeningsEenheid, Range<LocalDate> zoekPeriode)
	{
		return findWith(heeftScreeningsEenheidId(screeningsEenheid.getId()).with(MammaMinderValideReservering_.capaciteitBlok)
				.and(DateSpecification.bevatLocalDate(zoekPeriode, r -> r.get(MammaMinderValideReservering_.vanaf))),
			PlanningMinderValideReserveringProjectie.class,
			q -> q.projections(
					(cb, r) -> List.of(
						r.get(MammaMinderValideReservering_.id),
						SpecificationUtil.join(r, MammaMinderValideReservering_.capaciteitBlok).get(MammaCapaciteitBlok_.id),
						r.get(MammaMinderValideReservering_.vanaf)))
				.all());
	}
}
