package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDate;
import java.util.Collection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobespreking_;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingType;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaFotobesprekingSpecification
{
	public static Specification<MammaFotobespreking> filterOpGestartOpVanaf(LocalDate vanaf)
	{
		return skipWhenNull(vanaf, (r, q, cb) ->
			cb.greaterThan(cb.coalesce(r.get(MammaFotobespreking_.gestartOp), DateUtil.END_OF_TIME), DateUtil.toUtilDate(vanaf)));
	}

	public static Specification<MammaFotobespreking> filterOpType(Collection<MammaFotobesprekingType> types)
	{
		return skipWhenEmpty(types, (r, q, cb) -> r.get(MammaFotobespreking_.type).in(types));
	}

	public static Specification<MammaFotobespreking> filterOpBeoordelingseenheden(Collection<BeoordelingsEenheid> eenheden)
	{
		return skipWhenEmpty(eenheden, (r, q, cb) -> cb.or(
			r.get(MammaFotobespreking_.beoordelingsEenheid).in(eenheden),
			r.get(MammaFotobespreking_.beoordelingsEenheid).isNull()));
	}
}
