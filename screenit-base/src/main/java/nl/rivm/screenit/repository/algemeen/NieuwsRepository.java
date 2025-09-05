package nl.rivm.screenit.repository.algemeen;

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

import jakarta.persistence.criteria.JoinType;

import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.nieuws.MedewerkerNieuwsItem_;
import nl.rivm.screenit.model.nieuws.NieuwsItem;
import nl.rivm.screenit.model.nieuws.NieuwsItem_;
import nl.rivm.screenit.repository.BaseJpaRepository;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

public interface NieuwsRepository extends BaseJpaRepository<NieuwsItem>
{

	static Specification<NieuwsItem> baseSpecification()
	{
		return ((r, q, cb) -> cb.isNotNull(r));
	}

	static Specification<NieuwsItem> publicerenTot(Date date)
	{
		return ((r, q, cb) -> cb.or(cb.isNull(r.get(NieuwsItem_.publicerenTot)), cb.greaterThan(r.get(NieuwsItem_.publicerenTot), date)));
	}

	static Specification<NieuwsItem> publicerenVanaf(Date date)
	{
		return ((r, q, cb) -> cb.or(cb.isNull(r.get(NieuwsItem_.publicerenVanaf)), cb.lessThanOrEqualTo(r.get(NieuwsItem_.publicerenVanaf), date)));
	}

	static Specification<NieuwsItem> isOngelezen(Medewerker medewerker)
	{
		return ((nieuwsItem, q, cb) ->
		{
			var join = join(nieuwsItem, NieuwsItem_.medewerkerNieuwsItems, JoinType.LEFT);
			var medewerkerNieuwsItem = join.on(cb.equal(join.get(MedewerkerNieuwsItem_.medewerker), medewerker));

			var gemaakt = nieuwsItem.get(NieuwsItem_.gemaakt);
			var nietZichtbaarVanaf = medewerkerNieuwsItem.get(MedewerkerNieuwsItem_.nietZichtbaarVanaf);
			var gewijzigd = nieuwsItem.get(NieuwsItem_.gewijzigd);
			return cb.or(medewerkerNieuwsItem.isNull(),
				cb.and(cb.equal(medewerkerNieuwsItem.get(MedewerkerNieuwsItem_.medewerker), medewerker),
					cb.or(cb.isNull(nietZichtbaarVanaf),
						cb.or(
							cb.and(cb.isNull(gewijzigd),
								cb.greaterThan(gemaakt, nietZichtbaarVanaf)),
							cb.greaterThan(gewijzigd, nietZichtbaarVanaf)))));
		});
	}

}
