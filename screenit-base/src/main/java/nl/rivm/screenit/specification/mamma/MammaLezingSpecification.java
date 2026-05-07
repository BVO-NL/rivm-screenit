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
import java.time.LocalDateTime;
import java.util.List;

import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Path;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.model.mamma.MammaArchitectuurverstoringLaesie;
import nl.rivm.screenit.model.mamma.MammaAsymmetrieLaesie;
import nl.rivm.screenit.model.mamma.MammaCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaLezing_;
import nl.rivm.screenit.model.mamma.MammaMassaLaesie;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaArchitectuurverstoringCalcificatiesLaesie;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaArchitectuurverstoringMassaLaesie;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaBenigneKalk;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaConformLaesie;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaGeenBijzonderhedenLaesie;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaMarkering;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaMassaCalcificaties;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaMassaSpiculae;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaMassaSpiculaeCalcificaties;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaProjectiesLinksLaesie;
import nl.rivm.screenit.model.mamma.legacyLaesies.MammaProjectiesRechtsLaesie;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.DateSpecification.bevatLocalDateToDate;
import static nl.rivm.screenit.specification.SpecificationUtil.equalsOrFalseIfParamNull;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.notEqualsOrFalseIfParamNull;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaLezingSpecification
{

	public static ExtendedSpecification<MammaLezing> isAfwezigOfGedaanDoor(OrganisatieMedewerker radioloog)
	{
		return (r, q, cb) -> cb.or(
			r.isNull(),
			equalsOrFalseIfParamNull(r.get(MammaLezing_.beoordelaar), radioloog, cb));
	}

	public static ExtendedSpecification<MammaLezing> isGedaanDoor(OrganisatieMedewerker radioloog)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaLezing_.beoordelaar), radioloog);
	}

	public static ExtendedSpecification<MammaLezing> isGedaanDoor(Path<OrganisatieMedewerker> radioloog)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaLezing_.beoordelaar), radioloog);
	}

	public static ExtendedSpecification<MammaLezing> isNietGedaanDoor(OrganisatieMedewerker radioloog)
	{
		return (r, q, cb) -> notEqualsOrFalseIfParamNull(r.get(MammaLezing_.beoordelaar), radioloog, cb);
	}

	public static ExtendedSpecification<MammaLezing> isVerwezenDoor(OrganisatieMedewerker radioloog)
	{
		return isGedaanDoor(radioloog).and(isVerwezen());
	}

	public static ExtendedSpecification<MammaLezing> isVerwezen()
	{
		return (r, q, cb) -> cb.or(
			r.get(MammaLezing_.biradsLinks).in(MammaBIRADSWaarde.getVerwijzendBIRADSWaarden()),
			r.get(MammaLezing_.biradsRechts).in(MammaBIRADSWaarde.getVerwijzendBIRADSWaarden()));
	}

	public static ExtendedSpecification<MammaLezing> isGedaanInPeriode(Range<LocalDate> periode)
	{
		return bevatLocalDateToDate(periode, r -> r.get(MammaLezing_.beoordelingDatum));
	}

	public static ExtendedSpecification<MammaLezing> isGedaanBinnenTermijnDoor(OrganisatieMedewerker radioloog, LocalDate peildatum, Termijn termijn)
	{
		return isGedaanDoor(radioloog).and(isGedaanInPeriode(termijn.getPeriode(peildatum)));
	}

	public static ExtendedSpecification<MammaLezing> heeftBeoordelingDatumVanaf(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaLezing_.beoordelingDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaLezing> heeftBeoordelingDatumOpOfVoor(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaLezing_.beoordelingDatum), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaLezing> heeftNietBeoordeeldSindsSubquery(LocalDateTime peilMoment)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(MammaLezing.class);
			var subqueryRoot = subquery.from(MammaLezing.class);

			subquery.select(subqueryRoot).where(
				isGedaanDoor(r.get(MammaLezing_.beoordelaar)).and(heeftBeoordelingDatumVanaf(peilMoment))
					.toPredicate(subqueryRoot, q, cb));

			return cb.not(cb.exists(subquery));
		};
	}

	public static ExtendedSpecification<MammaLezing> filterOpRedenenFotobesprekingRadioloog(List<MammaLezingRedenenFotobesprekingRadioloog> redenen)
	{
		return skipWhenEmpty(redenen, (r, q, cb) -> join(r, MammaLezing_.redenenFotobesprekingRadioloog, JoinType.LEFT).in(redenen));
	}

	public static ExtendedSpecification<MammaLezing> filterOpRedenenFotobesprekingMbber(List<MammaLezingRedenenFotobesprekingMbber> redenen)
	{
		return skipWhenEmpty(redenen, (r, q, cb) -> join(r, MammaLezing_.redenenFotobesprekingMbber, JoinType.LEFT).in(redenen));
	}

	public static ExtendedSpecification<MammaLezing> filterOpLaesies(List<MammaLaesieType> laesieTypes)
	{
		return skipWhenEmpty(laesieTypes, (r, q, cb) ->
		{
			var laesieJoin = join(r, MammaLezing_.laesies, JoinType.LEFT);
			var laesieKlassen = laesieTypes.stream()
				.map(MammaLezingSpecification::naarLaesieKlasse)
				.toList();

			return laesieJoin.type().in(laesieKlassen);
		});
	}

	private static Class<? extends MammaLaesie> naarLaesieKlasse(MammaLaesieType type)
	{
		return switch (type)
		{
			case MASSA -> MammaMassaLaesie.class;
			case CALCIFICATIES -> MammaCalcificatiesLaesie.class;
			case ARCHITECTUURVERSTORING -> MammaArchitectuurverstoringLaesie.class;
			case ASYMMETRIE -> MammaAsymmetrieLaesie.class;
			case LEGACY_ARCHITECTUURVERSTORING_MET_CALCIFICATIES -> MammaArchitectuurverstoringCalcificatiesLaesie.class;
			case LEGACY_MASSA_MET_ARCHITECTUURVERSTORING -> MammaArchitectuurverstoringMassaLaesie.class;
			case LEGACY_CONFORM -> MammaConformLaesie.class;
			case LEGACY_GEEN_BIJZONDERHEDEN -> MammaGeenBijzonderhedenLaesie.class;
			case LEGACY_MASSA_MET_SPICULAE -> MammaMassaSpiculae.class;
			case LEGACY_PROJECTIE_NAAR_LINKS -> MammaProjectiesLinksLaesie.class;
			case LEGACY_PROJECTIE_NAAR_RECHTS -> MammaProjectiesRechtsLaesie.class;
			case LEGACY_MASSA_MET_CALCIFICATIES -> MammaMassaCalcificaties.class;
			case LEGACY_MASSA_MET_SPICULAE_EN_CALCIFICATIES -> MammaMassaSpiculaeCalcificaties.class;
			case LEGACY_MARKERING -> MammaMarkering.class;
			case LEGACY_BENIGNE_KALK -> MammaBenigneKalk.class;
			default -> throw new IllegalArgumentException("Onbekend laesietype: " + type);
		};
	}
}
