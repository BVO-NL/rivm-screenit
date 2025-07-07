package nl.rivm.screenit.specification.algemeen;

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
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.util.Pair;

import com.google.common.collect.BoundType;

import static nl.rivm.screenit.specification.DateSpecification.intervalInDagen;
import static nl.rivm.screenit.specification.RangeSpecification.bevat;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.exactCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MedewerkerSpecification
{
	public static Specification<Gebruiker> isActieveGebruiker(LocalDate vandaag, int dagenWachtwoordGeldig)
	{
		return (r, q, cb) ->
		{
			var peildatumLaatstGewijzigdDate = DateUtil.toUtilDate(vandaag.minusDays(dagenWachtwoordGeldig));

			var vanafExpression = cb.coalesce(r.get(Gebruiker_.actiefVanaf), DateUtil.BEGIN_OF_TIME);
			var totEnMetExpression = cb.coalesce(r.get(Gebruiker_.actiefTotEnMet), DateUtil.END_OF_TIME);
			var actiefTijdensVerlopenVanWachtwoord =
				cb.and(
					cb.lessThanOrEqualTo(vanafExpression, intervalInDagen(cb, r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), dagenWachtwoordGeldig)),
					cb.greaterThanOrEqualTo(totEnMetExpression, intervalInDagen(cb, r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), dagenWachtwoordGeldig)));
			var nuActief = isActiefOpMoment(vandaag.atStartOfDay()).toPredicate(r, q, cb);

			var wachtwoordVerlooptInDeToekomst = cb.greaterThan(r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), peildatumLaatstGewijzigdDate);
			return
				cb.and(
					cb.isTrue(r.get(Gebruiker_.actief)),
					cb.or(
						cb.and(
							wachtwoordVerlooptInDeToekomst,
							actiefTijdensVerlopenVanWachtwoord),
						cb.and(
							cb.not(wachtwoordVerlooptInDeToekomst),
							nuActief))
				);
		};
	}

	public static Specification<Gebruiker> heeftEmailAdres()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Gebruiker_.emailextra));
	}

	public static Specification<Gebruiker> filterEmailAdres(String emailAdres)
	{
		return skipWhenEmpty(emailAdres, (r, q, cb) -> exactCaseInsensitive(cb, r.get(Gebruiker_.emailextra), emailAdres));
	}

	public static Specification<Gebruiker> heeftWachtwoordInlogMethode()
	{
		return (r, q, cb) -> r.get(Gebruiker_.inlogMethode).in(List.of(InlogMethode.YUBIKEY, InlogMethode.GEBRUIKERSNAAM_WACHTWOORD));
	}

	public static Specification<Gebruiker> moetHerinneringKrijgen(LocalDate laatsteKeerWachtwoordGewijzigdPeildatum)
	{
		return (r, q, cb) ->
			cb.and(
				cb.lessThan(r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), DateUtil.toUtilDate(laatsteKeerWachtwoordGewijzigdPeildatum)),
				cb.isFalse(r.get(Gebruiker_.wachtwoordVerlooptWaarschuwingVerzonden))
			);
	}

	public static Specification<Gebruiker> isNietGeblokkeerd()
	{
		return (r, q, cb) -> cb.notEqual(r.get(Gebruiker_.inlogstatus), InlogStatus.GEBLOKKEERD);
	}

	public static ExtendedSpecification<Gebruiker> heeftHandtekening()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Gebruiker_.handtekening));
	}

	public static Specification<Gebruiker> isActiefTotEnMetVoor(LocalDate peilDatum)
	{
		return (r, q, cb) ->
		{
			var totEnMetExpression = cb.coalesce(r.get(Gebruiker_.actiefTotEnMet), DateUtil.END_OF_TIME);
			return cb.lessThan(totEnMetExpression, DateUtil.toUtilDate(peilDatum));
		};
	}

	public static Specification<Gebruiker> isActief(boolean waarde)
	{
		return (r, q, cb) -> cb.equal(r.get(Gebruiker_.actief), waarde);
	}

	public static ExtendedSpecification<Gebruiker> filterAchternaamContaining(String achternaam)
	{
		return skipWhenEmptyExtended(achternaam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(Gebruiker_.achternaam), achternaam));
	}

	public static ExtendedSpecification<Gebruiker> filterUzinummerContaining(String uzinummer)
	{
		return skipWhenEmptyExtended(uzinummer, (r, q, cb) -> containsCaseInsensitive(cb, r.get(Gebruiker_.uzinummer), uzinummer));
	}

	public static Specification<Gebruiker> filterMedewerkercode(Integer medewerkercode)
	{
		return skipWhenNull(medewerkercode, (r, q, cb) -> cb.equal(r.get(Gebruiker_.medewerkercode), medewerkercode));
	}

	public static ExtendedSpecification<Gebruiker> isActiefEnActiefOpMoment(LocalDateTime peilmoment)
	{
		return isActief().and(isActiefOpMoment(peilmoment));
	}

	public static ExtendedSpecification<Gebruiker> isActief()
	{
		return (r, q, cb) -> cb.isTrue(r.get(Gebruiker_.actief));
	}

	public static ExtendedSpecification<Gebruiker> filterActief(Boolean waarde)
	{
		return skipWhenNullExtended(waarde, (r, q, cb) -> cb.equal(r.get(Gebruiker_.actief), waarde));
	}

	public static ExtendedSpecification<Gebruiker> filterActiefVanaf(Date waarde)
	{
		return skipWhenNullExtended(waarde, (r, q, cb) -> cb.equal(r.get(Gebruiker_.actiefVanaf), waarde));
	}

	public static ExtendedSpecification<Gebruiker> filterActiefTotEnMet(Date waarde)
	{
		return skipWhenNullExtended(waarde, (r, q, cb) -> cb.equal(r.get(Gebruiker_.actiefTotEnMet), waarde));
	}

	private static @NotNull ExtendedSpecification<Gebruiker> isActiefOpMoment(LocalDateTime peilmoment)
	{
		return (r, q, cb) ->
		{
			var vanafExpression = cb.coalesce(r.get(Gebruiker_.actiefVanaf), DateUtil.BEGIN_OF_TIME);
			var totEnMetExpression = cb.coalesce(r.get(Gebruiker_.actiefTotEnMet), DateUtil.END_OF_TIME);

			var bevat = bevat(ri -> vanafExpression, ri -> totEnMetExpression, Pair.of(BoundType.CLOSED, BoundType.CLOSED), DateUtil.toUtilDate(peilmoment));
			return bevat.toPredicate(r, q, cb);
		};
	}

	public static Specification<Gebruiker> filterFunctieIn(Collection<Functie> functies)
	{
		return skipWhenEmpty(functies, (r, q, cb) -> r.get(Gebruiker_.functie).in(functies));
	}

	public static Specification<Gebruiker> filterFunctie(Functie functie)
	{
		return skipWhenNull(functie, (r, q, cb) -> cb.equal(r.get(Gebruiker_.functie), functie));
	}
}
