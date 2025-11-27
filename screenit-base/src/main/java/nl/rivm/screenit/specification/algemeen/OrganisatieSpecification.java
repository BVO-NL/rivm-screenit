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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.CentraleEenheid_;
import nl.rivm.screenit.model.Mammapoli;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Organisatie_;
import nl.rivm.screenit.model.RadiologieAfdeling;
import nl.rivm.screenit.model.Rivm;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.Woonplaats;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.ZorgInstelling_;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisarts_;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonIntakelocatie_;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie_;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.colon.PaLaboratorium_;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenOrganisatieOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenOrganisatieOvereenkomst_;
import nl.rivm.screenit.model.overeenkomsten.Overeenkomst;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.composePredicates;
import static nl.rivm.screenit.specification.SpecificationUtil.composePredicatesOr;
import static nl.rivm.screenit.specification.SpecificationUtil.containsCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.exactCaseInsensitive;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmptyExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenFalse;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.SpecificationUtil.treat;
import static nl.rivm.screenit.specification.algemeen.AdresSpecification.filterPostcodeContaining;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterAchternaamContaining;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterUzinummerContaining;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OrganisatieSpecification
{

	public static ExtendedSpecification<Organisatie> filterActief(Boolean actief)
	{
		return skipWhenNullExtended(actief, (r, q, cb) -> cb.equal(r.get(Organisatie_.actief), actief));
	}

	public static ExtendedSpecification<Organisatie> filterNaamContaining(String naam)
	{
		return skipWhenEmptyExtended(naam, (r, q, cb) -> containsCaseInsensitive(cb, r.get(Organisatie_.naam), naam));
	}

	public static ExtendedSpecification<Organisatie> filterEmailsDeelsExactEnDeelsContaining(String email)
	{
		return skipWhenEmptyExtended(email, (r, q, cb) ->
		{
			var ceRoot = treat(r, CentraleEenheid.class, cb);
			return cb.or(
				exactCaseInsensitive(cb, r.get(Organisatie_.email), email),
				exactCaseInsensitive(cb, ceRoot.get(CentraleEenheid_.email2), email),
				exactCaseInsensitive(cb, ceRoot.get(CentraleEenheid_.email3), email),
				exactCaseInsensitive(cb, ceRoot.get(CentraleEenheid_.email4), email),
				exactCaseInsensitive(cb, treat(r, ColonIntakelocatie.class, cb).get(ColonIntakelocatie_.emailSignaleringIntakelocatie), email),
				containsCaseInsensitive(cb, treat(r, CervixHuisarts.class, cb).get(CervixHuisarts_.extraEmails), email));
		});
	}

	public static ExtendedSpecification<Organisatie> filterId(Long id)
	{
		return skipWhenNullExtended(id, (r, q, cb) -> cb.equal(r.get(AbstractHibernateObject_.id), id));
	}

	public static Specification<Organisatie> heeftNietOrganisatieTypes(List<OrganisatieType> excludeOrganisatieTypes)
	{
		return skipWhenEmpty(excludeOrganisatieTypes, (r, q, cb) -> cb.not(r.get(Organisatie_.organisatieType).in(excludeOrganisatieTypes)));
	}

	public static Specification<Organisatie> heeftOrganisatieTypes(List<OrganisatieType> organisatieTypes)
	{
		return (r, q, cb) -> r.get(Organisatie_.organisatieType).in(organisatieTypes);
	}

	public static ExtendedSpecification<Organisatie> heeftOrganisatieType(OrganisatieType organisatieType)
	{
		return (r, q, cb) -> cb.equal(r.get(Organisatie_.organisatieType), organisatieType);
	}

	public static Specification<Organisatie> filterUniekeCodeContaining(String uniekeCode)
	{
		return skipWhenEmpty(uniekeCode, (r, q, cb) -> cb.or(
			containsCaseInsensitive(cb, r.get(Organisatie_.uziAbonneenummer), uniekeCode),
			containsCaseInsensitive(cb, r.get(Organisatie_.agbcode), uniekeCode),
			containsCaseInsensitive(cb, r.get(Organisatie_.rootOid), uniekeCode)
		));
	}

	public static ExtendedSpecification<Organisatie> filterAgbCodeOfUziAbonneenummerContaining(String uniekeCode)
	{
		return skipWhenEmptyExtended(uniekeCode, (r, q, cb) -> cb.or(
			containsCaseInsensitive(cb, r.get(Organisatie_.uziAbonneenummer), uniekeCode),
			containsCaseInsensitive(cb, r.get(Organisatie_.agbcode), uniekeCode)
		));
	}

	public static Specification<Organisatie> filterUziAbonneeNummer(String uziAbonneeNummer)
	{
		return skipWhenEmpty(uziAbonneeNummer, (r, q, cb) -> cb.equal(r.get(Organisatie_.uziAbonneenummer), uziAbonneeNummer));
	}

	public static Specification<Organisatie> filterOrganisatieType(OrganisatieType organisatieType)
	{
		return skipWhenNull(organisatieType, (r, q, cb) -> cb.equal(r.get(Organisatie_.organisatieType), organisatieType));
	}

	public static ExtendedSpecification<Organisatie> filterOvereenkomst(Overeenkomst overeenkomst, Date peildatum)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var subqueryRoot = subquery.from(AfgeslotenOrganisatieOvereenkomst.class);
			var organisatieJoin = join(subqueryRoot, AfgeslotenOrganisatieOvereenkomst_.organisatie);
			var predicates = new ArrayList<Predicate>();
			if (overeenkomst != null)
			{
				predicates.add(AbstractAfgeslotenOvereenkomstSpecification.heeftOvereenkomst(overeenkomst).toPredicate(subqueryRoot, q, cb));
			}
			if (peildatum != null)
			{
				predicates.add(AbstractAfgeslotenOvereenkomstSpecification.bevatPeildatum(peildatum).toPredicate(subqueryRoot, q, cb));
			}

			subquery.select(organisatieJoin.get(AbstractHibernateObject_.id));
			if (!predicates.isEmpty())
			{
				subquery.where(composePredicates(cb, predicates));
			}

			return r.get(AbstractHibernateObject_.id).in(subquery);
		};
	}

	public static Specification<Organisatie> filterParent(ScreeningOrganisatie parent)
	{
		return skipWhenNull(parent, (r, q, cb) ->
		{
			var parentJoin = join(r, Organisatie_.parent, JoinType.LEFT);
			return cb.or(cb.equal(r.get(Organisatie_.parent), parent), cb.equal(parentJoin.get(Organisatie_.parent), parent));
		});
	}

	public static ExtendedSpecification<Organisatie> filterAdres(String plaats, String postcode)
	{
		return skipWhenFalse(StringUtils.isNotBlank(plaats) || StringUtils.isNotBlank(postcode),
			AdresSpecification.filterPlaatsPostcode(plaats, postcode).with(Organisatie_.adres, JoinType.LEFT)
				.or(AdresSpecification.filterPlaatsPostcode(plaats, postcode).with(Organisatie_.postbusAdres, JoinType.LEFT))
				.or(AdresSpecification.filterPlaatsPostcode(plaats, postcode).with(Organisatie_.antwoordnummerAdres, JoinType.LEFT)));
	}

	public static ExtendedSpecification<Organisatie> filterPlaatsnaam(String plaatsnaam)
	{
		return filterAdres(plaatsnaam, null);
	}

	public static Specification<Organisatie> heeftFqdn(String fqdn)
	{
		return (r, q, cb) -> cb.or(cb.equal(treat(r, ZorgInstelling.class, cb).get(ZorgInstelling_.fqdn), fqdn),
			cb.equal(treat(r, ZorgInstelling.class, cb).get(ZorgInstelling_.fqdn), fqdn), cb.equal(treat(r, PaLaboratorium.class, cb).get(PaLaboratorium_.fqdn), fqdn),
			cb.equal(treat(r, ColoscopieLocatie.class, cb).get(ColoscopieLocatie_.fqdn), fqdn));
	}

	public static Specification<Organisatie> getZoekOrganisatiesSpecification(Organisatie organisatie, Map<OrganisatieType, List<Organisatie>> hierarchieCriteria,
		List<OrganisatieType> excludeOrganisatieTypes, LocalDateTime peilMoment)
	{
		return (r, q, cb) ->
		{
			var predicates = new ArrayList<Predicate>();
			predicates.add(forceerDtypesVoorAlleMogelijkeOrganisationTypes(r, cb));
			predicates.add(hierarchiePredicate(hierarchieCriteria, r, cb));

			var spec = filterActief(organisatie.getActief())
				.and(filterNaamContaining(organisatie.getNaam()))
				.and(filterEmailsDeelsExactEnDeelsContaining(organisatie.getEmail()))
				.and(filterId(organisatie.getId()))
				.and(heeftNietOrganisatieTypes(excludeOrganisatieTypes))
				.and(filterUniekeCodeContaining(organisatie.getUziAbonneenummer()))
				.and(getMedewerkerSpecifications(organisatie, peilMoment))
				.and(filterPlaatsPostcode(organisatie, cb));

			predicates.add(spec.toPredicate(r, q, cb));

			return composePredicates(cb, predicates);
		};
	}

	private static Predicate hierarchiePredicate(Map<OrganisatieType, List<Organisatie>> hierarchieCriteria, From<?, ? extends Organisatie> r, CriteriaBuilder cb)
	{
		if (MapUtils.isNotEmpty(hierarchieCriteria))
		{
			var disjunctionPredicates = new ArrayList<Predicate>();
			hierarchieCriteria.entrySet().forEach(type -> disjunctionPredicates.add(hierarchieEntryPredicate(type, r, cb)));
			return composePredicatesOr(cb, disjunctionPredicates);
		}
		return null;
	}

	public static ExtendedSpecification<Organisatie> filterHierarchie(Map<OrganisatieType, List<Organisatie>> hierarchieCriteria)
	{
		return (r, q, cb) -> hierarchiePredicate(hierarchieCriteria, r, cb);
	}

	private static Specification<Organisatie> getMedewerkerSpecifications(Organisatie organisatie, LocalDateTime peilMoment)
	{
		if (CollectionUtils.isNotEmpty(organisatie.getOrganisatieMedewerkers()))
		{
			var medewerker = organisatie.getOrganisatieMedewerkers().get(0).getMedewerker();
			if (StringUtils.isNotBlank(medewerker.getAchternaam()) || StringUtils.isNotBlank(medewerker.getUzinummer()))
			{
				return filterAchternaamContaining(medewerker.getAchternaam())
					.and(filterUzinummerContaining(medewerker.getUzinummer())
						.and(MedewerkerSpecification.isActiefEnActiefOpMoment(peilMoment))).with(medewerkerJoin())
					.and(OrganisatieMedewerkerSpecification.isActief().with(organisatieMedewerkerJoin()));
			}
		}
		return null;
	}

	private static Specification<Organisatie> filterPlaatsPostcode(Organisatie zoekObject, CriteriaBuilder cb)
	{
		var adres = zoekObject.getAdres();
		Specification<Organisatie> spec = null;
		if (adres != null)
		{
			if (adres.getPlaats() != null)
			{
				var plaatsFilter = AdresSpecification.filterPlaatsContaining(adres.getPlaats());
				spec = plaatsFilter.with(Organisatie_.adres, JoinType.LEFT)
					.or(plaatsFilter.with(Organisatie_.postbusAdres, JoinType.LEFT))
					.or(plaatsFilter.with(Organisatie_.antwoordnummerAdres, JoinType.LEFT))
					.or(WoonplaatsSpecification.filterPlaatsContaining(adres.getPlaats()).with(woonplaatsJoin(cb)));
			}
			if (adres.getPostcode() != null)
			{
				var postcodeFilter = filterPostcodeContaining(adres.getPostcode());
				spec = postcodeFilter.with(Organisatie_.adres, JoinType.LEFT)
					.or(postcodeFilter.with(Organisatie_.postbusAdres, JoinType.LEFT))
					.or(postcodeFilter.with(Organisatie_.antwoordnummerAdres, JoinType.LEFT))
					.or(postcodeFilter.with(ri -> postadresJoin(cb, ri))).and(spec);
			}
		}
		return spec;
	}

	private static Predicate forceerDtypesVoorAlleMogelijkeOrganisationTypes(Root<Organisatie> r, CriteriaBuilder cb)
	{
		var organisatiePredicates = new ArrayList<Predicate>();
		var mogelijkeOrganisatieClasses = List.of(Organisatie.class, BMHKLaboratorium.class, BeoordelingsEenheid.class, CentraleEenheid.class, CervixHuisarts.class,
			ColonIntakelocatie.class, ColoscopieLocatie.class, ColonFitLaboratorium.class, Mammapoli.class, PaLaboratorium.class, RadiologieAfdeling.class, Rivm.class,
			ScreeningOrganisatie.class, ZorgInstelling.class);
		mogelijkeOrganisatieClasses.forEach(c -> organisatiePredicates.add(cb.equal(treat(r, c, cb).type(), c)));
		return composePredicatesOr(cb, organisatiePredicates);
	}

	public static Predicate hierarchieEntryPredicate(Map.Entry<OrganisatieType, List<Organisatie>> type, From<?, ? extends Organisatie> root, CriteriaBuilder cb)
	{
		var predicates = new ArrayList<Predicate>();
		var organisatieType = type.getKey();
		var organisatiesVoorToegangslevel = type.getValue();
		predicates.add(cb.equal(root.get(Organisatie_.organisatieType), organisatieType));
		if (CollectionUtils.isNotEmpty(organisatiesVoorToegangslevel))
		{
			var organisatieTypeToegangslevel = organisatiesVoorToegangslevel.get(0).getOrganisatieType();
			var idPath = root.get(AbstractHibernateObject_.id);
			var isScreeningorganisatieToegangslevel = organisatieTypeToegangslevel == OrganisatieType.SCREENINGSORGANISATIE;
			switch (organisatieType)
			{
			case SCREENINGSORGANISATIE:
			case INPAKCENTRUM:
			case RIVM:
			case LABORATORIUM:
			case BMHK_LABORATORIUM:
			case HUISARTS:
				predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				break;
			case PA_LABORATORIUM:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(
						parentJoin(parentJoin(join(treat(root, PaLaboratorium.class, cb), PaLaboratorium_.coloscopielocaties, JoinType.LEFT))), organisatiesVoorToegangslevel, cb));
				}
				else
				{
					predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				}
				break;
			case INTAKELOCATIE:
			case COLOSCOPIELOCATIE:
			case MAMMAPOLI:
			case RADIOLOGIEAFDELING:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(parentJoin(parentJoin(root)), organisatiesVoorToegangslevel, cb));
				}
				else
				{
					predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				}
				break;
			case ZORGINSTELLING:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(parentJoin(root), organisatiesVoorToegangslevel, cb));
				}
				else
				{
					predicates.add(createCriteriaOrganisaties(idPath, organisatiesVoorToegangslevel, cb));
				}
				break;
			case BEOORDELINGSEENHEID:
				if (isScreeningorganisatieToegangslevel)
				{
					predicates.add(createCriteriaOrganisaties(join(parentJoin(root), Organisatie_.regio, JoinType.LEFT), organisatiesVoorToegangslevel, cb));
				}
				break;
			default:
				break;
			}
		}
		return composePredicates(cb, predicates);
	}

	public static Specification<Organisatie> heeftNaam(String naam)
	{
		return (r, q, cb) -> cb.equal(r.get(Organisatie_.naam), naam);
	}

	public static <T extends Organisatie> ExtendedSpecification<T> isActief(boolean actief)
	{
		return (r, q, cb) -> cb.equal(r.get(Organisatie_.actief), actief);
	}

	public static <T extends Organisatie> ExtendedSpecification<T> isActieveOrganisatie(Class<? extends Organisatie> typeOrganisatie)
	{
		return (r, q, cb) ->
		{
			var organisatie = treat(r, typeOrganisatie, cb);
			return cb.isTrue(organisatie.get(Organisatie_.actief));
		};
	}

	public static <T extends Organisatie> ExtendedSpecification<T> heeftParent(Organisatie organisatie, Class<? extends Organisatie> typeOrganisatie)
	{
		return (r, q, cb) ->
		{
			var root = treat(r, typeOrganisatie, cb);
			return cb.equal(root.get(Organisatie_.parent), organisatie);
		};
	}

	public static <T extends Organisatie> ExtendedSpecification<T> heeftColoscopielocatieId(Long locatieId)
	{
		return (r, q, cb) ->
		{
			var root = treat(r, PaLaboratorium.class, cb);
			var coloscopielocatieJoin = join(root, PaLaboratorium_.coloscopielocaties);
			return cb.equal(coloscopielocatieJoin.get(AbstractHibernateObject_.id), locatieId);
		};
	}

	public static <T extends Organisatie> ExtendedSpecification<T> heeftColoscopielocatieParent(Long locatieId)
	{
		return (r, q, cb) ->
		{
			var root = treat(r, PaLaboratorium.class, cb);
			var coloscopielocatieJoin = join(root, PaLaboratorium_.coloscopielocaties);
			return cb.equal(coloscopielocatieJoin.get(Organisatie_.parent).get(AbstractHibernateObject_.id), locatieId);
		};
	}

	public static <T extends Organisatie> ExtendedSpecification<T> heeftRegio(Organisatie regio)
	{
		return (r, q, cb) -> cb.equal(r.get(Organisatie_.regio), regio);
	}

	public static Specification<Organisatie> heeftUziAbonneenummer(String uziAbonneenummer)
	{
		return (r, q, cb) -> cb.equal(r.get(Organisatie_.uziAbonneenummer), uziAbonneenummer);
	}

	public static Specification<Organisatie> heeftRootOid(String rootOid)
	{
		return (r, q, cb) -> cb.equal(r.get(Organisatie_.rootOid), rootOid);
	}

	@NotNull
	private static Function<From<?, ? extends Organisatie>, From<?, ? extends OrganisatieMedewerker>> organisatieMedewerkerJoin()
	{
		return r -> join(r, Organisatie_.organisatieMedewerkers);
	}

	@NotNull
	private static Function<From<?, ? extends Organisatie>, From<?, ? extends Medewerker>> medewerkerJoin()
	{
		return r ->
		{
			var organisatieMedewerkerJoin = organisatieMedewerkerJoin().apply(r);
			return join(organisatieMedewerkerJoin, OrganisatieMedewerker_.medewerker);
		};
	}

	@NotNull
	private static Join<CervixHuisarts, CervixHuisartsAdres> postadresJoin(CriteriaBuilder cb, From<?, ? extends Organisatie> r)
	{
		return join(treat(r, CervixHuisarts.class, cb), CervixHuisarts_.postadres, JoinType.LEFT);
	}

	@NotNull
	private static Function<From<?, ? extends Organisatie>, From<?, ? extends Woonplaats>> woonplaatsJoin(CriteriaBuilder cb)
	{
		return r -> join(postadresJoin(cb, r), CervixHuisartsAdres_.woonplaats, JoinType.LEFT);
	}

	@NotNull
	private static Join<? extends Organisatie, ? extends Organisatie> parentJoin(From<?, ? extends Organisatie> root)
	{
		return join(root, Organisatie_.parent, JoinType.LEFT);
	}

	private static Predicate createCriteriaOrganisaties(Path<?> path, List<Organisatie> organisaties, CriteriaBuilder cb)
	{
		if (organisaties.size() == 1)
		{
			if (path.getJavaType().equals(Long.class))
			{
				return cb.equal(path, organisaties.get(0).getId());
			}
			else
			{
				return cb.equal(path, organisaties.get(0));
			}
		}
		else
		{
			return path.in(organisaties);
		}
	}
}
