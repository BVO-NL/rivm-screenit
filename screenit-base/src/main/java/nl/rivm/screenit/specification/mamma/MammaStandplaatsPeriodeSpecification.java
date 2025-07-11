package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde_;
import nl.rivm.screenit.model.mamma.MammaStandplaats_;
import nl.rivm.screenit.specification.DateSpecification;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.organisatie.model.Adres_;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;
import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaStandplaatsPeriodeSpecification
{

	public static Specification<MammaStandplaatsPeriode> heeftActieveStandplaatsPeriode(MammaStandplaats standplaats, Date peilDatumTijd)
	{
		return (r, q, cb) ->
		{
			var standplaatsJoin = standplaatsJoin(r);
			var screeningsEenheidJoin = join(r, MammaStandplaatsPeriode_.screeningsEenheid);
			var beoordelingsEenheidJoin = join(screeningsEenheidJoin, MammaScreeningsEenheid_.beoordelingsEenheid);
			var centraleEenheidJoin = join(beoordelingsEenheidJoin, Instelling_.parent);

			return cb.and(
				cb.equal(standplaatsJoin, standplaats),
				cb.isTrue(standplaatsJoin.get(MammaStandplaats_.actief)),
				cb.isTrue(centraleEenheidJoin.get(Instelling_.actief)),
				cb.greaterThanOrEqualTo(r.get(MammaStandplaatsPeriode_.totEnMet), peilDatumTijd)
			);
		};
	}

	public static Specification<MammaStandplaatsPeriode> filterScreeningsEenheden(List<MammaScreeningsEenheid> screeningsEenheden)
	{
		return skipWhenEmpty(screeningsEenheden, (r, q, cb) -> join(r, MammaStandplaatsPeriode_.screeningsEenheid).in(screeningsEenheden));
	}

	public static Specification<MammaStandplaatsPeriode> heeftActieveScreeningsEenheid()
	{
		return (r, q, cb) ->
		{
			var screeningsEenheidJoin = join(r, MammaStandplaatsPeriode_.screeningsEenheid);
			return cb.isTrue(screeningsEenheidJoin.get(MammaScreeningsEenheid_.actief));
		};
	}

	public static Specification<MammaStandplaatsPeriode> heeftActieveStandplaats()
	{
		return (r, q, cb) ->
		{
			var standplaatsJoin = standplaatsJoin(r);
			return cb.isTrue(standplaatsJoin.get(MammaStandplaats_.actief));
		};
	}

	public static Specification<MammaStandplaatsPeriode> overlaptMetPeriode(Range<LocalDate> periode)
	{
		return DateSpecification.overlaptLocalDateToDate(periode, r -> r.get(MammaStandplaatsPeriode_.vanaf), r -> r.get(MammaStandplaatsPeriode_.totEnMet));
	}

	public static Specification<MammaStandplaatsPeriode> heeftPlaats(String plaats)
	{
		return (r, q, cb) ->
		{
			var standplaatsJoin = standplaatsJoin(r);
			var locatieJoin = join(standplaatsJoin, MammaStandplaats_.locatie);
			return cb.equal(locatieJoin.get(Adres_.plaats), plaats);
		};
	}

	public static Specification<MammaStandplaatsPeriode> heeftStandplaats(List<MammaStandplaats> standplaatsen)
	{
		return (r, q, cb) ->
		{
			var standplaatsRondeJoin = join(r, MammaStandplaatsPeriode_.standplaatsRonde);
			return standplaatsRondeJoin.get(MammaStandplaatsRonde_.standplaats).in(standplaatsen);
		};
	}

	public static Specification<MammaStandplaatsPeriode> isBeschikbaarVoorScreeningsOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return (r, q, cb) ->
		{
			var standplaatsRondeJoin = join(r, MammaStandplaatsPeriode_.standplaatsRonde);
			var standplaatsJoin = join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaats);
			var capaciteitBeschikbaarVoorJoin = join(standplaatsRondeJoin, MammaStandplaatsRonde_.afspraakcapaciteitBeschikbaarVoor, JoinType.LEFT);
			return cb.or(
				cb.equal(standplaatsJoin.get(MammaStandplaats_.regio), screeningOrganisatie),
				cb.equal(capaciteitBeschikbaarVoorJoin, screeningOrganisatie)
			);
		};
	}

	public static Specification<MammaStandplaatsPeriode> heeftBeoordelingsEenheidEnStandplaatsGekoppeldAanScreeningOrganisatie(ScreeningOrganisatie so)
	{
		return MammaScreeningsEenheidSpecification.heeftScreeningOrganisatie(so).with(MammaStandplaatsPeriode_.screeningsEenheid)
			.and(MammaStandplaatsSpecification.heeftScreeningOrganisatieId(so).with(root -> standplaatsJoin(root)));
	}

	public static Specification<MammaStandplaatsPeriode> begintOpOfNaVrijgegevenTotEnMetDatum()
	{
		return (r, q, cb) ->
		{
			var screeningsEenheidJoin = join(r, MammaStandplaatsPeriode_.screeningsEenheid);
			return cb.greaterThanOrEqualTo(screeningsEenheidJoin.get(MammaScreeningsEenheid_.vrijgegevenTotEnMet), r.get(MammaStandplaatsPeriode_.vanaf));
		};
	}

	public static ExtendedSpecification<MammaStandplaatsPeriode> eindigtOpOfNaDatum(LocalDate datum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaStandplaatsPeriode_.totEnMet), DateUtil.toUtilDate(datum));
	}

	public static Specification<MammaStandplaatsPeriode> heeftScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		return heeftScreeningsEenheidId(Optional.ofNullable(screeningsEenheid).map(MammaScreeningsEenheid::getId).orElse(null));
	}

	public static Specification<MammaStandplaatsPeriode> heeftScreeningsEenheidId(Long screeningsEenheidId)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaStandplaatsPeriode_.screeningsEenheid).get(AbstractHibernateObject_.id), screeningsEenheidId);
	}

	public static Specification<MammaStandplaatsPeriode> isActiefOpDatum(Date datum)
	{
		return bevat(r -> r.get(MammaStandplaatsPeriode_.vanaf), r -> r.get(MammaStandplaatsPeriode_.totEnMet), Pair.of(BoundType.CLOSED, BoundType.CLOSED), datum);
	}

	public static Specification<MammaStandplaatsPeriode> heeftScreeningsEenheidVolgNrVanaf(int volgnummer)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(r.get(MammaStandplaatsPeriode_.screeningsEenheidVolgNr), volgnummer);
	}

	private static Join<MammaStandplaatsRonde, MammaStandplaats> standplaatsJoin(From<?, ? extends MammaStandplaatsPeriode> r)
	{
		var standplaatsRondeJoin = join(r, MammaStandplaatsPeriode_.standplaatsRonde);
		return join(standplaatsRondeJoin, MammaStandplaatsRonde_.standplaats);
	}
}
