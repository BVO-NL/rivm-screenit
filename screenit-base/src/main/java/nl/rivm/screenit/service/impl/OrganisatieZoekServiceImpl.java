package nl.rivm.screenit.service.impl;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Root;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.Woonplaats_;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsAdres_;
import nl.rivm.screenit.model.cervix.CervixHuisarts_;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieCentrumWrapper;
import nl.rivm.screenit.model.colon.ColoscopieCentrumZoekCriteria;
import nl.rivm.screenit.model.colon.PaLaboratorium;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.repository.algemeen.OrganisatieRepository;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.OrganisatieZoekService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.collections.CollectionUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.model.Organisatie_.ADRES;
import static nl.rivm.screenit.model.cervix.CervixHuisartsAdres_.WOONPLAATS;
import static nl.rivm.screenit.model.cervix.CervixHuisarts_.POSTADRES;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.treat;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.getZoekOrganisatiesSpecification;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftFqdn;
import static nl.rivm.screenit.util.StringUtil.propertyChain;
import static nl.topicuszorg.organisatie.model.Adres_.PLAATS;
import static nl.topicuszorg.organisatie.model.Adres_.STRAAT;

@Service
public class OrganisatieZoekServiceImpl implements OrganisatieZoekService
{
	@Autowired
	private OrganisatieService organisatieService;

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private OrganisatieRepository organisatieRepository;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public List<Organisatie> zoekOrganisaties(Organisatie searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		OrganisatieMedewerker organisatieMedewerker, long first, long count, String sortProperty, boolean asc)
	{
		var hierarchieCriteria = getHierarchieCriteria(searchObject, selectedOrganisatieTypes, organisatieMedewerker);
		return zoekOrganisaties(searchObject, hierarchieCriteria, excludeOrganisatieTypes, first, count, sortProperty, asc);
	}

	private List<Organisatie> zoekOrganisaties(Organisatie searchObject, Map<OrganisatieType, List<Organisatie>> hierarchieCriteria, List<OrganisatieType> excludeOrganisatieTypes,
		long first, long count, String sortProperty, boolean asc)
	{
		var spec = getZoekOrganisatiesSpecification(searchObject, hierarchieCriteria, excludeOrganisatieTypes, currentDateSupplier.getLocalDateTime());

		var alleGevondenOrganisaties = organisatieRepository.findWith(spec, q -> q.sortBy(getSort(sortProperty, asc), this::addJoinsForSortingOrCreateDedicatedOrders)).all();
		var alleUniekeGevondenOrganisaties = new ArrayList<>(new LinkedHashSet<>(alleGevondenOrganisaties));
		List<Organisatie> resultaten = alleUniekeGevondenOrganisaties;
		if (first != -1)
		{
			if (alleUniekeGevondenOrganisaties.size() > first + count)
			{
				resultaten = alleUniekeGevondenOrganisaties.subList((int) first, (int) (first + count));
			}
			else if (alleUniekeGevondenOrganisaties.size() > first)
			{
				resultaten = alleUniekeGevondenOrganisaties.subList((int) first, alleUniekeGevondenOrganisaties.size());
			}
		}
		return resultaten;
	}

	private @NotNull Sort getSort(String sortProperty, boolean asc)
	{
		var direction = asc ? Sort.Direction.ASC : Sort.Direction.DESC;
		var sort = Sort.by(direction, sortProperty);
		if (sortProperty.startsWith(propertyChain(ADRES, PLAATS)))
		{
			sort = sort.and(Sort.by(direction, propertyChain(POSTADRES, WOONPLAATS, Woonplaats_.NAAM)));
		}
		else if (sortProperty.startsWith(propertyChain(ADRES, STRAAT)))
		{
			sort = sort.and(Sort.by(direction, propertyChain(POSTADRES, STRAAT)));
		}
		sort = sort.and(Sort.by(direction, AbstractHibernateObject_.ID));
		return sort;
	}

	private jakarta.persistence.criteria.Order addJoinsForSortingOrCreateDedicatedOrders(Sort.Order order, Root<Organisatie> r, CriteriaBuilder cb)
	{
		var sortProperty = order.getProperty();
		if (sortProperty.startsWith(POSTADRES))
		{
			var postadresJoin = join(treat(r, CervixHuisarts.class, cb), CervixHuisarts_.postadres, JoinType.LEFT);
			Join<?, ?> propertyJoin = postadresJoin;
			String simpleProperty;
			if (sortProperty.startsWith(propertyChain(POSTADRES, WOONPLAATS)))
			{
				propertyJoin = join(postadresJoin, CervixHuisartsAdres_.woonplaats, JoinType.LEFT);
				simpleProperty = sortProperty.substring(propertyChain(POSTADRES, WOONPLAATS).length() + 1);
			}
			else
			{
				simpleProperty = sortProperty.substring(POSTADRES.length() + 1);
			}
			var exp = propertyJoin.get(simpleProperty);
			return order.isAscending() ? cb.asc(exp) : cb.desc(exp);
		}
		return null;
	}

	@Override
	public long countOrganisaties(Organisatie searchObject, List<OrganisatieType> selectedOrganisatieTypes, List<OrganisatieType> excludeOrganisatieTypes,
		OrganisatieMedewerker organisatieMedewerker)
	{
		var hierarchieCriteria = getHierarchieCriteria(searchObject, selectedOrganisatieTypes, organisatieMedewerker);
		var spec = getZoekOrganisatiesSpecification(searchObject, hierarchieCriteria, excludeOrganisatieTypes, currentDateSupplier.getLocalDateTime());
		return organisatieRepository.countDistinct(spec);
	}

	private Map<OrganisatieType, List<Organisatie>> getHierarchieCriteria(Organisatie zoekOrganisatie, List<OrganisatieType> selectedOrganisatieTypes,
		OrganisatieMedewerker organisatieMedewerker)
	{
		var hierarchieCriteria = new HashMap<OrganisatieType, List<Organisatie>>();
		var organisatieTypeGekozen = zoekOrganisatie.getOrganisatieType();
		if (organisatieTypeGekozen != null)
		{
			hierarchieCriteria.put(organisatieTypeGekozen, getOrganisatiesForGekozenOrganisatieType(organisatieMedewerker, organisatieTypeGekozen));
		}
		else if (CollectionUtils.isNotEmpty(selectedOrganisatieTypes))
		{
			for (var type : selectedOrganisatieTypes)
			{
				hierarchieCriteria.put(type, getOrganisatiesForGekozenOrganisatieType(organisatieMedewerker, type));
			}
		}
		return hierarchieCriteria;
	}

	private List<Organisatie> getOrganisatiesForGekozenOrganisatieType(OrganisatieMedewerker organisatieMedewerker, OrganisatieType organisatieTypeGekozen)
	{
		var toegangLevel = autorisatieService.getToegangLevel(organisatieMedewerker, Actie.INZIEN, true, organisatieTypeGekozen.getRecht());

		return getOrganisatiesForNiveau(organisatieMedewerker, organisatieTypeGekozen, toegangLevel);
	}

	@Override
	public List<Organisatie> getOrganisatiesForNiveau(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, OrganisatieType organisatieTypeGekozen, ToegangLevel toegangLevel)
	{
		var organisaties = new ArrayList<Organisatie>();
		var ingelogdVoorOrganisatie = ingelogdeOrganisatieMedewerker.getOrganisatie();
		switch (organisatieTypeGekozen)
		{
		case BMHK_LABORATORIUM:
		case ZORGINSTELLING:
		case INTAKELOCATIE:
		case COLOSCOPIELOCATIE:
		case PA_LABORATORIUM:
			switch (toegangLevel)
			{
			case ORGANISATIE:
				organisaties.add(ingelogdVoorOrganisatie);
				break;
			case REGIO:
				organisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(ingelogdVoorOrganisatie));
				break;
			default:
				break;
			}
			break;

		case SCREENINGSORGANISATIE:
			if (toegangLevel.equals(ToegangLevel.REGIO))
			{
				organisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(ingelogdVoorOrganisatie));
			}
			break;

		default:
			break;
		}
		return organisaties;
	}

	@Override
	public List<Organisatie> screeningsorganisatiesWaarOrganisatieOndervalt(Organisatie organisatie)
	{
		var screeningsorganisaties = new ArrayList<Organisatie>();
		if (OrganisatieType.PA_LABORATORIUM == organisatie.getOrganisatieType())
		{
			for (var locatie : ((PaLaboratorium) organisatie).getColoscopielocaties())
			{
				screeningsorganisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(locatie));
			}
		}
		else if (OrganisatieType.SCREENINGSORGANISATIE == organisatie.getOrganisatieType())
		{
			screeningsorganisaties.add(organisatie);
		}
		else
		{
			var parent = organisatie.getParent();
			if (parent != null)
			{
				screeningsorganisaties.addAll(screeningsorganisatiesWaarOrganisatieOndervalt(parent));
			}
		}
		return screeningsorganisaties;
	}

	@Override
	public List<ColoscopieCentrumWrapper> zoekIntakeLocaties(ColoscopieCentrumZoekCriteria zoekObject, Client client, boolean alleenActiefKamers)
	{
		Map<OrganisatieType, List<Organisatie>> types = Map.of(OrganisatieType.INTAKELOCATIE, List.of());

		var searchObject = new Organisatie();
		searchObject.setNaam(zoekObject.getNaam());
		searchObject.setAdres(new Adres());
		searchObject.getAdres().setPlaats(zoekObject.getPlaats());
		var organisaties = zoekOrganisaties(searchObject, types, null, -1, -1, "naam", true);
		var list = new ArrayList<ColoscopieCentrumWrapper>();

		var coordinaten = coordinatenService.getCoordinatenVanPersoon(client.getPersoon());

		for (var organisatie : organisaties)
		{
			var intakelocatie = (ColonIntakelocatie) organisatie;
			if (alleenActiefKamers)
			{
				var alleKamersInactief = true;
				for (var kamer : intakelocatie.getKamers())
				{
					if (kamer.getActief())
					{
						alleKamersInactief = false;
						break;
					}
				}
				if (alleKamersInactief)
				{
					continue;
				}
			}

			var wrapper = new ColoscopieCentrumWrapper();
			wrapper.setNaam(intakelocatie.getNaam());
			var adres = intakelocatie.getAdres();
			var postbusAdres = intakelocatie.getPostbusAdres();
			if (adres != null && adres.getPlaats() != null)
			{
				wrapper.setPlaats(adres.getPlaats());
			}
			if (postbusAdres != null && wrapper.getPlaats() == null && postbusAdres.getPlaats() != null)
			{
				wrapper.setPlaats(postbusAdres.getPlaats());
			}
			wrapper.setId(intakelocatie.getId());

			var to = intakelocatie.getPostcodeCoordinaten();
			if (coordinaten.vanAdres != null && to != null)
			{
				var distance = BigDecimalUtil.berekenDistance(coordinaten.vanAdres, to);
				if (zoekObject.getAfstand() != null && zoekObject.getAfstand().doubleValue() < distance)
				{
					continue;
				}
				else
				{
					wrapper.setAfstand(BigDecimal.valueOf(distance));
				}
			}

			list.add(wrapper);
		}
		return list;
	}

	@Override
	public List<Organisatie> getMogelijkeParents(Organisatie organisatie, @NotNull OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		if (organisatie.getOrganisatieType() == OrganisatieType.BEOORDELINGSEENHEID)
		{
			return getMogelijkeParentsVoorBeoordelingsEenheid(ingelogdeOrganisatieMedewerker);
		}

		Class<? extends Organisatie> filter = null;
		switch (organisatie.getOrganisatieType())
		{
		case ZORGINSTELLING:
			filter = ScreeningOrganisatie.class;
			break;
		case MAMMAPOLI:
		case RADIOLOGIEAFDELING:
		case INTAKELOCATIE:
		case COLOSCOPIELOCATIE:
			filter = ZorgInstelling.class;
			break;
		default:
			break;
		}

		return (List<Organisatie>) organisatieService.getActieveOrganisaties(filter);
	}

	private List<Organisatie> getMogelijkeParentsVoorBeoordelingsEenheid(OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		var ingelogdeOrganisatie = ingelogdeOrganisatieMedewerker.getOrganisatie();
		if (OrganisatieType.SCREENINGSORGANISATIE == ingelogdeOrganisatie.getOrganisatieType())
		{
			var screeningOrganisatie = (ScreeningOrganisatie) ingelogdeOrganisatie;
			return organisatieService.getActieveCentraleEenhedenBinnenRegio(screeningOrganisatie).stream().map(ce -> (Organisatie) ce).toList();
		}
		else
		{
			return new ArrayList<>();
		}
	}

	@Override
	public List<Organisatie> getAllActieveOrganisatiesWithType(Class<? extends Organisatie> organisatie)
	{
		return (List<Organisatie>) organisatieService.getActieveOrganisaties(organisatie);
	}

	@Override
	public List<Long> getZichtbareOrganisatiesOpToegangLevel(Organisatie organisatie, ToegangLevel level, List<OrganisatieType> types)
	{
		var zichtbaar = new ArrayList<Long>();
		var regios = screeningsorganisatiesWaarOrganisatieOndervalt(organisatie);
		if (ToegangLevel.REGIO.equals(level) && CollectionUtils.isNotEmpty(regios) && !types.isEmpty())
		{
			var hierarchieCriteria = new HashMap<OrganisatieType, List<Organisatie>>();
			for (var type : types)
			{
				hierarchieCriteria.put(type, regios);
			}
			var result = zoekOrganisaties(new Organisatie(), hierarchieCriteria, null, -1, -1, "id", true);
			var regioIds = new ArrayList<Long>();
			result.forEach(i -> regioIds.add(i.getId()));
			zichtbaar.addAll(regioIds);
		}
		else if (ToegangLevel.LANDELIJK.equals(level) && !types.isEmpty())
		{
			for (var type : types)
			{
				zichtbaar.addAll(organisatieService.getOrganisatieIdsMetType(type));
			}
		}
		else
		{
			zichtbaar.add(organisatie.getId());
		}
		return zichtbaar;
	}

	@Override
	public ColoscopieCentrumWrapper getNearestIntakeLocatie(Client client)
	{
		var zoekObject = new ColoscopieCentrumZoekCriteria();
		var intakeLocaties = zoekIntakeLocaties(zoekObject, client, true);
		ColoscopieCentrumWrapper ilZonderAfstand = null;
		ColoscopieCentrumWrapper ilMetAfstand = null;
		for (var wrapper : intakeLocaties)
		{
			if (ilZonderAfstand == null && wrapper.getAfstand() == null)
			{
				ilZonderAfstand = wrapper;
			}
			else if (wrapper.getAfstand() != null && (ilMetAfstand == null || ilMetAfstand.getAfstand() != null && wrapper.getAfstand().compareTo(ilMetAfstand.getAfstand()) < 0))
			{
				ilMetAfstand = wrapper;
			}
		}
		if (ilMetAfstand != null)
		{
			return ilMetAfstand;
		}
		else
		{
			return ilZonderAfstand;
		}
	}

	@Override
	public List<Organisatie> zoekOrganisatieMetFqdn(String fqdn)
	{
		return organisatieRepository.findAll(filterActief(true).and(heeftFqdn(fqdn)));
	}
}
