package nl.rivm.screenit.specification.algemeen;

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
import java.util.Date;
import java.util.List;

import jakarta.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.Brief_;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief_;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import static nl.rivm.screenit.specification.DateSpecification.truncateToLocalDate;
import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.joinByString;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class BriefSpecification
{
	public static <B extends Brief> ExtendedSpecification<B> heeftNietBriefType(BriefType briefType)
	{
		return (r, q, cb) -> cb.notEqual(r.get(Brief_.briefType), briefType);
	}

	public static <B extends Brief> ExtendedSpecification<B> heeftBriefType(BriefType briefType)
	{
		return (r, q, cb) -> cb.equal(r.get(Brief_.briefType), briefType);
	}

	public static <B extends Brief> ExtendedSpecification<B> heeftBriefTypeIn(List<BriefType> briefTypes)
	{
		return (r, q, cb) -> r.get(Brief_.briefType).in(briefTypes);
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietVervangen()
	{
		return (r, q, cb) -> cb.isFalse(r.get(Brief_.vervangen));
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietGegenereerd()
	{
		return (r, q, cb) -> cb.isFalse(r.get(Brief_.gegenereerd));
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietTegengehouden()
	{
		return (r, q, cb) -> cb.isFalse(r.get(Brief_.tegenhouden));
	}

	public static <B extends Brief> ExtendedSpecification<B> isGegenereerd(boolean isGegenereerd)
	{
		return (r, q, cb) -> cb.equal(r.get(Brief_.gegenereerd), isGegenereerd);
	}

	public static <B extends Brief> ExtendedSpecification<B> isAangemaaktVoor(LocalDate peilmoment)
	{
		return (r, q, cb) -> cb.lessThan(r.get(Brief_.creatieDatum), DateUtil.toUtilDate(peilmoment));
	}

	public static <B extends Brief> ExtendedSpecification<B> isAangemaaktVoorOfOp(Date peildatum)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(Brief_.creatieDatum), peildatum);
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietVerstuurdVoorAfdrukken()
	{
		return not(isVerstuurdVoorAfdrukken());
	}

	public static <B extends Brief> ExtendedSpecification<B> isVerstuurdVoorAfdrukken()
	{
		ExtendedSpecification<B> mergedBrievenAfgedrukt = MergedBrievenSpecification.isVerstuurd()
			.with(r -> joinByString(r, AlgemeneBrief_.MERGED_BRIEVEN, JoinType.LEFT));
		return mergedBrievenAfgedrukt.or(heeftVerstuurdVoorAfdrukkenOp());
	}

	private static <B extends Brief> ExtendedSpecification<B> heeftVerstuurdVoorAfdrukkenOp()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Brief_.verstuurdVoorAfdrukkenOp));
	}

	public static <B extends Brief> ExtendedSpecification<B> isVerstuurdVoorAfdrukkenVoor(LocalDate datum)
	{
		ExtendedSpecification<B> mergedBrievenAfgedrukt = MergedBrievenSpecification.heeftPrintDatumVoor(datum)
			.with(r -> joinByString(r, AlgemeneBrief_.MERGED_BRIEVEN, JoinType.LEFT));
		return mergedBrievenAfgedrukt.or(heeftVerstuurdVoorAfdrukkenVoor(datum));
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietVerstuurdVoorAfdrukken(LocalDate peildatum)
	{
		ExtendedSpecification<B> mergedBrievenNietAfgedrukt = MergedBrievenSpecification.isNietGeprintOp(peildatum)
			.with(r -> joinByString(r, AlgemeneBrief_.MERGED_BRIEVEN, JoinType.LEFT));
		return mergedBrievenNietAfgedrukt.and(isNietVerstuurdVoorAfdrukkenOp(peildatum));
	}

	public static <B extends Brief> ExtendedSpecification<B> isNietVerstuurdVoorAfdrukkenOp(LocalDate peildatum)
	{
		return (r, q, cb) -> cb.or(cb.isNull(r.get(Brief_.verstuurdVoorAfdrukkenOp)),
			cb.notEqual(truncateToLocalDate("day", r.get(Brief_.verstuurdVoorAfdrukkenOp), cb), peildatum));
	}

	private static <B extends Brief> ExtendedSpecification<B> heeftVerstuurdVoorAfdrukkenVoor(LocalDate peilDatum)
	{
		return (r, q, cb) -> cb.lessThan(r.get(Brief_.verstuurdVoorAfdrukkenOp), peilDatum.atStartOfDay());
	}

}
