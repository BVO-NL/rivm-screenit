package nl.rivm.screenit.main.service.algemeen.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.service.algemeen.MedewerkerZoekService;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Medewerker_;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol_;
import nl.rivm.screenit.model.OrganisatieMedewerker_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.MedewerkerRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieSpecification;
import nl.rivm.screenit.specification.algemeen.RolSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import com.google.common.primitives.Ints;

import static jakarta.persistence.criteria.JoinType.INNER;
import static jakarta.persistence.criteria.JoinType.LEFT;
import static nl.rivm.screenit.model.OrganisatieType.HUISARTS;
import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.filterId;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenEmpty;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenFalse;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterAchternaamContaining;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterEmailAdres;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterFunctie;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterFunctieIn;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterMedewerkercode;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.filterUzinummerContaining;
import static nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification.heeftBevolkingsonderzoekIn;
import static nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification.heeftRolIn;
import static nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification.isActiefOpDatum;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.filterAgbCodeOfUziAbonneenummerContaining;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.filterNaamContaining;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftOrganisatieType;

@Service
@RequiredArgsConstructor
public class MedewerkerZoekServiceImpl implements MedewerkerZoekService
{
	private final OrganisatieZoekService organisatieZoekService;

	private final AutorisatieService autorisatieService;

	private final MedewerkerRepository medewerkerRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Medewerker> searchMedewerkers(Medewerker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, OrganisatieMedewerker ingelogdeOrganisatieMedewerker,
		boolean voorOrganisatieKoppelen, long first, long count, Sort sort)
	{
		var zoekSpecification = getZoekSpecification(zoekObject, selectedFuncties, selectedRollen, ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen);
		return medewerkerRepository.findWith(zoekSpecification, q -> q.sortBy(sort).all(first, count));
	}

	@Override
	public long countMedewerkers(Medewerker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, OrganisatieMedewerker ingelogdeOrganisatieMedewerker,
		boolean voorOrganisatieKoppelen)
	{
		var zoekSpecification = getZoekSpecification(zoekObject, selectedFuncties, selectedRollen, ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen);
		return medewerkerRepository.count(zoekSpecification);
	}

	private Specification<Medewerker> getZoekSpecification(Medewerker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker, boolean voorOrganisatieKoppelen)
	{
		if (voorOrganisatieKoppelen)
		{
			return getMedewerkerKoppelenSpecification(zoekObject);
		}
		return getMedewerkerZoekenSpecification(zoekObject, selectedFuncties, selectedRollen, ingelogdeOrganisatieMedewerker);
	}

	private Specification<Medewerker> getMedewerkerKoppelenSpecification(Medewerker zoekObject)
	{
		return filterBasisVelden(zoekObject)
			.and(filterFunctie(zoekObject.getFunctie()))
			.and(filterOrganisatieVoorMedewerkerKoppelen(getOrganisatie(zoekObject)));
	}

	private ExtendedSpecification<Medewerker> filterBasisVelden(Medewerker zoekObject)
	{
		return filterActief(zoekObject.getActief())
			.and(filterId(zoekObject.getId()))
			.and(filterAchternaamContaining(zoekObject.getAchternaam()));
	}

	public static Specification<Medewerker> filterOrganisatieVoorMedewerkerKoppelen(Organisatie organisatie)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var medewerkerRoot = subquery.from(Medewerker.class);

			var geenHuisartsOfGeenOrganisatieKoppelingen = not(heeftOrganisatieType(HUISARTS)).or(HibernateObjectSpecification.heeftGeenId());
			var organisatieSpecification = organisatie == null ? geenHuisartsOfGeenOrganisatieKoppelingen : HibernateObjectSpecification.<Organisatie> heeftId(organisatie.getId());

			subquery.where(organisatieSpecification.<Medewerker> with(s -> getOrganisatiesJoin(s, LEFT))
				.toPredicate(medewerkerRoot, q, cb));

			subquery.select(medewerkerRoot.get(AbstractHibernateObject_.id));
			return r.get(AbstractHibernateObject_.id).in(subquery);
		};
	}

	private Specification<Medewerker> getMedewerkerZoekenSpecification(Medewerker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		return filterBasisVelden(zoekObject)
			.and(filterEmailAdres(zoekObject.getEmailextra()))
			.and(filterMedewerkercodeOfUzinummer(zoekObject.getUzinummer()))
			.and(filterFunctieIn(selectedFuncties))
			.and(filterOrganisatieMedewerker(zoekObject, selectedRollen, ingelogdeOrganisatieMedewerker)
				.or(includeerNietGekoppeldeMedewerkers(selectedRollen, zoekObject)));
	}

	private static Specification<Medewerker> filterMedewerkercodeOfUzinummer(String code)
	{
		if (code != null)
		{
			return filterUzinummerContaining(code).or(filterMedewerkercode(Ints.tryParse(code)));
		}
		return null;
	}

	public Specification<Medewerker> filterOrganisatieMedewerker(Medewerker zoekObject, List<Rol> selectedRollen, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var organisatieMedewerkerRoot = subquery.from(OrganisatieMedewerker.class);

			subquery.where(OrganisatieMedewerkerSpecification.isActief()
				.and(heeftBevolkingsonderzoekIn(ingelogdeOrganisatieMedewerker.getBevolkingsonderzoeken()).with(OrganisatieMedewerker_.rollen))
				.and(filterRol(selectedRollen))
				.and(filterOrganisatie(zoekObject))
				.and(filterHierarchie(ingelogdeOrganisatieMedewerker))
				.toPredicate(organisatieMedewerkerRoot, q, cb));

			subquery.select(organisatieMedewerkerRoot.get(OrganisatieMedewerker_.medewerker).get(AbstractHibernateObject_.id));
			return r.get(AbstractHibernateObject_.id).in(subquery);
		};
	}

	private ExtendedSpecification<OrganisatieMedewerker> filterRol(List<Rol> rollen)
	{
		return skipWhenEmpty(rollen,
			isActiefOpDatum(currentDateSupplier.getLocalDate())
				.and(heeftRolIn(rollen)).with(OrganisatieMedewerker_.rollen)
				.and(RolSpecification.isActief(true).with(s -> getRollenJoin(s))));
	}

	private ExtendedSpecification<OrganisatieMedewerker> filterOrganisatie(Medewerker zoekObject)
	{
		var organisatie = getOrganisatie(zoekObject);
		return skipWhenFalse(isOrganisatieFilterGevuld(organisatie),
			OrganisatieSpecification.isActief(true)
				.and(filterNaamContaining(organisatie.getNaam()))
				.and(filterAgbCodeOfUziAbonneenummerContaining(organisatie.getUziAbonneenummer()))
				.and(OrganisatieSpecification.filterPlaatsnaam(organisatie.getAdres().getPlaats()))
				.with(OrganisatieMedewerker_.organisatie)
		);
	}

	private boolean isOrganisatieFilterGevuld(Organisatie organisatie)
	{
		return StringUtils.isNotBlank(organisatie.getNaam()) || StringUtils.isNotBlank(organisatie.getUziAbonneenummer()) || StringUtils.isNotBlank(
			organisatie.getAdres().getPlaats());
	}

	private ExtendedSpecification<OrganisatieMedewerker> filterHierarchie(OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		var hierarchieCriteria = getHierarchieCriteria(ingelogdeOrganisatieMedewerker);
		var hierarchieCriteriaGevuld = MapUtils.isNotEmpty(hierarchieCriteria);
		return skipWhenFalse(hierarchieCriteriaGevuld,
			OrganisatieSpecification.isActief(true)
				.and(OrganisatieSpecification.filterHierarchie(hierarchieCriteria))
				.with(OrganisatieMedewerker_.organisatie)
		);
	}

	private Map<OrganisatieType, List<Organisatie>> getHierarchieCriteria(OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		Map<OrganisatieType, List<Organisatie>> hierarchieCriteria = new EnumMap<>(OrganisatieType.class);
		var toegangLevel = autorisatieService.getToegangLevel(ingelogdeOrganisatieMedewerker, Actie.INZIEN, true, Recht.MEDEWERKER_BEHEER);
		for (var organisatieType : OrganisatieType.values())
		{
			if (moetToegevoegdWordenAanHierarchieCriteria(organisatieType, toegangLevel.getNiveau()))
			{
				hierarchieCriteria.put(organisatieType, organisatieZoekService.getOrganisatiesForNiveau(ingelogdeOrganisatieMedewerker, organisatieType, toegangLevel));
			}
		}
		return hierarchieCriteria;
	}

	private boolean moetToegevoegdWordenAanHierarchieCriteria(OrganisatieType organisatieType, int toegangsniveau)
	{
		return switch (organisatieType)
		{
			case RIVM, INPAKCENTRUM, LABORATORIUM -> toegangsniveau == ToegangLevel.LANDELIJK.getNiveau();
			case SCREENINGSORGANISATIE -> toegangsniveau >= ToegangLevel.REGIO.getNiveau();
			default -> toegangsniveau >= ToegangLevel.ORGANISATIE.getNiveau();
		};
	}

	public Specification<Medewerker> includeerNietGekoppeldeMedewerkers(List<Rol> selectedRollen, Medewerker zoekObject)
	{
		var includeerNietGekoppeldeMedewerkers = CollectionUtils.isEmpty(selectedRollen) && !isOrganisatieFilterGevuld(getOrganisatie(zoekObject));
		return skipWhenFalse(includeerNietGekoppeldeMedewerkers, (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var medewerkerRoot = subquery.from(Medewerker.class);

			var heeftActieveRolKoppeling = OrganisatieMedewerkerSpecification.isActief().with(Medewerker_.organisatieMedewerkers)
				.and(OrganisatieSpecification.isActief(true).with(s -> getOrganisatiesJoin(s, INNER)))
				.and(heeftId().with(s -> getOrganisatieMedewerkerRollenJoin(s)));

			subquery.where(
				heeftActieveRolKoppeling
					.or(heeftOrganisatieType(HUISARTS).with(s -> getOrganisatiesJoin(s, INNER)))
					.toPredicate(medewerkerRoot, q, cb));

			subquery.select(medewerkerRoot.get(AbstractHibernateObject_.id)).distinct(true);
			return cb.not(r.get(AbstractHibernateObject_.id).in(subquery));
		});
	}

	private Organisatie getOrganisatie(Medewerker zoekObject)
	{
		return zoekObject.getOrganisatieMedewerkers().stream().findFirst().map(OrganisatieMedewerker::getOrganisatie).orElse(null);
	}

	private static Join<OrganisatieMedewerkerRol, Rol> getRollenJoin(From<?, ? extends OrganisatieMedewerker> organisatieMedewerkerRoot)
	{
		var rollenJoin = join(organisatieMedewerkerRoot, OrganisatieMedewerker_.rollen);
		return join(rollenJoin, OrganisatieMedewerkerRol_.rol);
	}

	private static Join<OrganisatieMedewerker, OrganisatieMedewerkerRol> getOrganisatieMedewerkerRollenJoin(From<?, ? extends Medewerker> medewerkerRoot)
	{
		return join(join(medewerkerRoot, Medewerker_.organisatieMedewerkers), OrganisatieMedewerker_.rollen, LEFT);
	}

	private static Join<OrganisatieMedewerker, Organisatie> getOrganisatiesJoin(From<?, ? extends Medewerker> medewerkerRoot, JoinType joinType)
	{
		return join(join(medewerkerRoot, Medewerker_.organisatieMedewerkers, joinType), OrganisatieMedewerker_.organisatie, joinType);
	}
}
