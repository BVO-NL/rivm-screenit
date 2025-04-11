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

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.service.algemeen.MedewerkerZoekService;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.model.InstellingGebruikerRol_;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.GebruikerRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieSpecification;
import nl.rivm.screenit.specification.algemeen.RolSpecification;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import com.google.common.primitives.Ints;

import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;

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

	private final GebruikerRepository gebruikerRepository;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Gebruiker> searchMedewerkers(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, InstellingGebruiker ingelogdeOrganisatieMedewerker,
		boolean voorOrganisatieKoppelen, long first, long count, Sort sort)
	{
		var zoekSpecification = getZoekSpecification(zoekObject, selectedFuncties, selectedRollen, ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen);
		return gebruikerRepository.findWith(zoekSpecification, q -> q.sortBy(sort).all(first, count));
	}

	@Override
	public long countMedewerkers(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen, InstellingGebruiker ingelogdeOrganisatieMedewerker,
		boolean voorOrganisatieKoppelen)
	{
		var zoekSpecification = getZoekSpecification(zoekObject, selectedFuncties, selectedRollen, ingelogdeOrganisatieMedewerker, voorOrganisatieKoppelen);
		return gebruikerRepository.count(zoekSpecification);
	}

	private Specification<Gebruiker> getZoekSpecification(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		InstellingGebruiker ingelogdeOrganisatieMedewerker, boolean voorOrganisatieKoppelen)
	{
		if (voorOrganisatieKoppelen)
		{
			return getMedewerkerKoppelenSpecification(zoekObject);
		}
		return getMedewerkerZoekenSpecification(zoekObject, selectedFuncties, selectedRollen, ingelogdeOrganisatieMedewerker);
	}

	private Specification<Gebruiker> getMedewerkerKoppelenSpecification(Gebruiker zoekObject)
	{
		return filterBasisVelden(zoekObject)
			.and(filterFunctie(zoekObject.getFunctie()))
			.and(filterOrganisatieVoorMedewerkerKoppelen(getOrganisatie(zoekObject)));
	}

	private ExtendedSpecification<Gebruiker> filterBasisVelden(Gebruiker zoekObject)
	{
		return filterActief(zoekObject.getActief())
			.and(filterId(zoekObject.getId()))
			.and(filterAchternaamContaining(zoekObject.getAchternaam()));
	}

	public static Specification<Gebruiker> filterOrganisatieVoorMedewerkerKoppelen(Instelling organisatie)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var medewerkerRoot = subquery.from(Gebruiker.class);

			var geenHuisartsOfGeenOrganisatieKoppelingen = not(heeftOrganisatieType(HUISARTS)).or(HibernateObjectSpecification.heeftGeenId());
			var organisatieSpecification = organisatie == null ? geenHuisartsOfGeenOrganisatieKoppelingen : HibernateObjectSpecification.<Instelling> heeftId(organisatie.getId());

			subquery.where(organisatieSpecification.<Gebruiker> with(s -> getOrganisatiesJoin(s, LEFT))
				.toPredicate(medewerkerRoot, q, cb));

			subquery.select(medewerkerRoot.get(SingleTableHibernateObject_.id));
			return r.in(subquery);
		};
	}

	private Specification<Gebruiker> getMedewerkerZoekenSpecification(Gebruiker zoekObject, List<Functie> selectedFuncties, List<Rol> selectedRollen,
		InstellingGebruiker ingelogdeOrganisatieMedewerker)
	{
		return filterBasisVelden(zoekObject)
			.and(filterEmailAdres(zoekObject.getEmailextra()))
			.and(filterMedewerkercodeOfUzinummer(zoekObject.getUzinummer()))
			.and(filterFunctieIn(selectedFuncties))
			.and(filterOrganisatieMedewerker(zoekObject, selectedRollen, ingelogdeOrganisatieMedewerker)
				.or(includeerNietGekoppeldeMedewerkers(selectedRollen, zoekObject)));
	}

	private static Specification<Gebruiker> filterMedewerkercodeOfUzinummer(String code)
	{
		if (code != null)
		{
			return filterUzinummerContaining(code).or(filterMedewerkercode(Ints.tryParse(code)));
		}
		return null;
	}

	public Specification<Gebruiker> filterOrganisatieMedewerker(Gebruiker zoekObject, List<Rol> selectedRollen, InstellingGebruiker ingelogdeOrganisatieMedewerker)
	{
		return (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var organisatieMedewerkerRoot = subquery.from(InstellingGebruiker.class);

			subquery.where(OrganisatieMedewerkerSpecification.isActief()
				.and(heeftBevolkingsonderzoekIn(ingelogdeOrganisatieMedewerker.getBevolkingsonderzoeken()).with(InstellingGebruiker_.rollen))
				.and(filterRol(selectedRollen))
				.and(filterOrganisatie(zoekObject))
				.and(filterHierarchie(ingelogdeOrganisatieMedewerker))
				.toPredicate(organisatieMedewerkerRoot, q, cb));

			subquery.select(organisatieMedewerkerRoot.get(InstellingGebruiker_.medewerker).get(SingleTableHibernateObject_.id));
			return r.in(subquery);
		};
	}

	private ExtendedSpecification<InstellingGebruiker> filterRol(List<Rol> rollen)
	{
		return skipWhenEmpty(rollen,
			isActiefOpDatum(currentDateSupplier.getLocalDate())
				.and(heeftRolIn(rollen)).with(InstellingGebruiker_.rollen)
				.and(RolSpecification.isActief(true).with(s -> getRollenJoin(s))));
	}

	private ExtendedSpecification<InstellingGebruiker> filterOrganisatie(Gebruiker zoekObject)
	{
		var organisatie = getOrganisatie(zoekObject);
		return skipWhenFalse(isOrganisatieFilterGevuld(organisatie),
			OrganisatieSpecification.isActief(true)
				.and(filterNaamContaining(organisatie.getNaam()))
				.and(filterAgbCodeOfUziAbonneenummerContaining(organisatie.getUziAbonneenummer()))
				.and(OrganisatieSpecification.filterPlaatsnaam(organisatie.getHuidigAdres().getPlaats()))
				.with(InstellingGebruiker_.organisatie)
		);
	}

	private boolean isOrganisatieFilterGevuld(Instelling organisatie)
	{
		return StringUtils.isNotBlank(organisatie.getNaam()) || StringUtils.isNotBlank(organisatie.getUziAbonneenummer()) || StringUtils.isNotBlank(
			organisatie.getHuidigAdres().getPlaats());
	}

	private ExtendedSpecification<InstellingGebruiker> filterHierarchie(InstellingGebruiker ingelogdeOrganisatieMedewerker)
	{
		var hierarchieCriteria = getHierarchieCriteria(ingelogdeOrganisatieMedewerker);
		var hierarchieCriteriaGevuld = MapUtils.isNotEmpty(hierarchieCriteria);
		return skipWhenFalse(hierarchieCriteriaGevuld,
			OrganisatieSpecification.isActief(true)
				.and(OrganisatieSpecification.filterHierarchie(hierarchieCriteria))
				.with(InstellingGebruiker_.organisatie)
		);
	}

	private Map<OrganisatieType, List<Instelling>> getHierarchieCriteria(InstellingGebruiker ingelogdeOrganisatieMedewerker)
	{
		Map<OrganisatieType, List<Instelling>> hierarchieCriteria = new EnumMap<>(OrganisatieType.class);
		var toegangLevel = autorisatieService.getToegangLevel(ingelogdeOrganisatieMedewerker, Actie.INZIEN, true, Recht.GEBRUIKER_MEDEWERKER_BEHEER);
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
			default -> toegangsniveau >= ToegangLevel.INSTELLING.getNiveau();
		};
	}

	public Specification<Gebruiker> includeerNietGekoppeldeMedewerkers(List<Rol> selectedRollen, Gebruiker zoekObject)
	{
		var includeerNietGekoppeldeMedewerkers = CollectionUtils.isEmpty(selectedRollen) && !isOrganisatieFilterGevuld(getOrganisatie(zoekObject));
		return skipWhenFalse(includeerNietGekoppeldeMedewerkers, (r, q, cb) ->
		{
			var subquery = q.subquery(Long.class);
			var medewerkerRoot = subquery.from(Gebruiker.class);

			var heeftActieveRolKoppeling = OrganisatieMedewerkerSpecification.isActief().with(Gebruiker_.organisatieMedewerkers)
				.and(OrganisatieSpecification.isActief(true).with(s -> getOrganisatiesJoin(s, INNER)))
				.and(heeftId().with(s -> getOrganisatieMedewerkerRollenJoin(s)));

			subquery.where(
				heeftActieveRolKoppeling
					.or(heeftOrganisatieType(HUISARTS).with(s -> getOrganisatiesJoin(s, INNER)))
					.toPredicate(medewerkerRoot, q, cb));

			subquery.select(medewerkerRoot.get(SingleTableHibernateObject_.id)).distinct(true);
			return cb.not(r.in(subquery));
		});
	}

	private Instelling getOrganisatie(Gebruiker zoekObject)
	{
		return zoekObject.getOrganisatieMedewerkers().stream().findFirst().map(InstellingGebruiker::getOrganisatie).orElse(null);
	}

	private static Join<InstellingGebruikerRol, Rol> getRollenJoin(From<?, ? extends InstellingGebruiker> organisatieMedewerkerRoot)
	{
		var rollenJoin = join(organisatieMedewerkerRoot, InstellingGebruiker_.rollen);
		return join(rollenJoin, InstellingGebruikerRol_.rol);
	}

	private static Join<InstellingGebruiker, InstellingGebruikerRol> getOrganisatieMedewerkerRollenJoin(From<?, ? extends Gebruiker> medewerkerRoot)
	{
		return join(join(medewerkerRoot, Gebruiker_.organisatieMedewerkers), InstellingGebruiker_.rollen, LEFT);
	}

	private static Join<InstellingGebruiker, Instelling> getOrganisatiesJoin(From<?, ? extends Gebruiker> medewerkerRoot, JoinType joinType)
	{
		return join(join(medewerkerRoot, Gebruiker_.organisatieMedewerkers, joinType), InstellingGebruiker_.organisatie, joinType);
	}
}
