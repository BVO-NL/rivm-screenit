package nl.rivm.screenit.main.service.impl;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.function.Function;
import java.util.stream.Collectors;

import jakarta.persistence.criteria.From;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.dto.OrganisatieMedewerkerRolDto;
import nl.rivm.screenit.main.dao.MedewerkerDao;
import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Medewerker_;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol_;
import nl.rivm.screenit.model.OrganisatieMedewerker_;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.Rol;
import nl.rivm.screenit.model.Rol_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.repository.FluentJpaQuery;
import nl.rivm.screenit.repository.algemeen.MedewerkerRepository;
import nl.rivm.screenit.repository.algemeen.OrganisatieMedewerkerRepository;
import nl.rivm.screenit.repository.algemeen.OrganisatieMedewerkerRolRepository;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.MedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.PermissieSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.MedewerkerSpecification.heeftHandtekening;
import static nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification.filterActief;
import static nl.rivm.screenit.specification.algemeen.PermissieSpecification.heeftRecht;

@Service
@AllArgsConstructor
public class MedewerkerServiceImpl implements MedewerkerService
{
	private final MedewerkerDao medewerkerDao;

	private final HibernateService hibernateService;

	private final AuthenticatieService authenticatieService;

	private final UploadDocumentService uploadDocumentService;

	private final OrganisatieMedewerkerRepository organisatieMedewerkerRepository;

	private final OrganisatieMedewerkerRolRepository organisatieMedewerkerRolRepository;

	private final MedewerkerRepository medewerkerRepository;

	private final LogService logService;

	@Override
	public List<OrganisatieMedewerker> zoekOrganisatieMedewerkers(OrganisatieMedewerker zoekOrganisatieMedewerker, long first, long count, Sort sort)
	{
		var spec = getSpecificationVoorOrganisatieMedewerker(zoekOrganisatieMedewerker);
		return organisatieMedewerkerRepository.findWith(spec, OrganisatieMedewerker.class, q -> q.sortBy(sort))
			.fetch(g ->
			{
				g.addSubgraph(OrganisatieMedewerker_.medewerker);
				g.addSubgraph(OrganisatieMedewerker_.organisatie);
			})
			.all(first, count); 
	}

	@Override
	public List<OrganisatieMedewerker> getActieveRadiologen(OrganisatieMedewerker zoekOrganisatieMedewerker, List<Long> exclIds, Sort sort)
	{
		var spec = getSpecificationVoorOrganisatieMedewerker(zoekOrganisatieMedewerker);

		if (!exclIds.isEmpty())
		{
			spec = spec.and(HibernateObjectSpecification.heeftNietIdIn(exclIds));
		}

		spec = spec
			.and(heeftHandtekening().with(OrganisatieMedewerker_.medewerker))
			.and(heeftRecht(Recht.MEDEWERKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST).with(permissieJoin()));

		return organisatieMedewerkerRepository.findWith(spec, OrganisatieMedewerker.class, q -> q.sortBy(sort))
			.fetch(g -> g.addSubgraph(OrganisatieMedewerker_.MEDEWERKER))
			.all();  
	}

	@Override
	public long countOrganisatieMedewerkers(OrganisatieMedewerker zoekOrganisatieMedewerker)
	{
		var spec = getSpecificationVoorOrganisatieMedewerker(zoekOrganisatieMedewerker);
		return organisatieMedewerkerRepository.countDistinct(spec);
	}

	@Override
	public void addOrganisatieMedewerker(Organisatie organisatie, Medewerker medewerker)
	{

		var organisatieMedewerker = getOrganisatieMedewerker(organisatie, medewerker);

		if (organisatieMedewerker != null)
		{

			if (Boolean.FALSE.equals(organisatieMedewerker.getActief()))
			{
				organisatieMedewerker.setActief(Boolean.TRUE);
				medewerkerDao.saveOrUpdateOrganisatieMedewerker(organisatieMedewerker);
			}
		}
		else
		{

			organisatieMedewerker = new OrganisatieMedewerker();
			organisatieMedewerker.setActief(Boolean.TRUE);
			organisatieMedewerker.setOrganisatie(organisatie);
			organisatieMedewerker.setMedewerker(medewerker);
			organisatieMedewerker.setRollen(new ArrayList<OrganisatieMedewerkerRol>());
			if (medewerker.getOrganisatieMedewerkers() == null)
			{
				medewerker.setOrganisatieMedewerkers(new ArrayList<OrganisatieMedewerker>());
			}
			medewerker.getOrganisatieMedewerkers().add(organisatieMedewerker);
			if (organisatie.getOrganisatieMedewerkers() == null)
			{
				organisatie.setOrganisatieMedewerkers(new ArrayList<OrganisatieMedewerker>());
			}
			organisatie.getOrganisatieMedewerkers().add(organisatieMedewerker);
			medewerkerDao.saveOrUpdateOrganisatieMedewerker(organisatieMedewerker);
		}
	}

	@Override
	@Transactional
	public void saveOrUpdateRollen(
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker, List<OrganisatieMedewerkerRolDto> initieleRollen, OrganisatieMedewerker organisatieMedewerker)
	{
		medewerkerDao.saveOrUpdateOrganisatieMedewerker(organisatieMedewerker);
		organisatieMedewerker.getRollen().forEach(rol -> saveLogInformatieVoorGewijzigdeRol(ingelogdeOrganisatieMedewerker, initieleRollen, rol, organisatieMedewerker));
		hibernateService.saveOrUpdateAll(organisatieMedewerker.getRollen());
	}

	private void saveLogInformatieVoorGewijzigdeRol(OrganisatieMedewerker ingelogdeOrganisatieMedewerker, List<OrganisatieMedewerkerRolDto> initieleRollen,
		OrganisatieMedewerkerRol rol,
		OrganisatieMedewerker organisatieMedewerker)
	{
		var huidigeBevolkingsonderzoeken = Bevolkingsonderzoek.getAfkortingen(rol.getBevolkingsonderzoeken());

		var melding = String.format("Medewerker: %s. Organisatie: %s. Rol '%s' met BVO('s) %s, beginDatum %s, eindDatum %s en status %s ",
			organisatieMedewerker.getMedewerker().getNaamVolledig(), organisatieMedewerker.getOrganisatie().getNaam(), rol.getRol().getNaam(), huidigeBevolkingsonderzoeken,
			maakRolDatumString(rol.getBeginDatum()), maakRolDatumString(rol.getEindDatum()), rol.getActief() ? "actief" : "inactief");

		var initieleRolOptional = initieleRollen.stream().filter(r -> Objects.equals(r.getId(), rol.getId())).findFirst();

		if (initieleRolOptional.isEmpty())
		{
			melding += "toegevoegd";
			logService.logGebeurtenis(LogGebeurtenis.MEDEWERKER_WIJZIG, ingelogdeOrganisatieMedewerker, melding);
			return;
		}

		var initieleRol = initieleRolOptional.get();
		var initieleBevolkingsonderzoeken = Bevolkingsonderzoek.getAfkortingen(initieleRol.getBevolkingsonderzoeken());

		if (!isRolGewijzigd(initieleRol, initieleBevolkingsonderzoeken, rol, huidigeBevolkingsonderzoeken))
		{
			return;
		}
		melding += "gewijzigd ";
		var wijzigingen = new StringJoiner(", ", "(", ")");

		if (!huidigeBevolkingsonderzoeken.equals(initieleBevolkingsonderzoeken))
		{
			wijzigingen.add(String.format("Bevolkingsonderzoeken: %s -> %s", initieleBevolkingsonderzoeken, huidigeBevolkingsonderzoeken));
		}
		if (!Objects.equals(rol.getBeginDatum(), initieleRol.getBeginDatum()))
		{
			wijzigingen.add(String.format("Begindatum: %s -> %s", maakRolDatumString(initieleRol.getBeginDatum()), maakRolDatumString(rol.getBeginDatum())));
		}
		if (!Objects.equals(rol.getEindDatum(), initieleRol.getEindDatum()))
		{
			wijzigingen.add(String.format("Einddatum: %s -> %s", maakRolDatumString(initieleRol.getEindDatum()), maakRolDatumString(rol.getEindDatum())));
		}
		if (rol.getActief() != initieleRol.getActief())
		{
			wijzigingen.add(String.format("Status: %s -> %s", initieleRol.getActief() ? "actief" : "inactief", rol.getActief() ? "actief" : "inactief"));
		}
		melding += wijzigingen.toString();
		logService.logGebeurtenis(LogGebeurtenis.MEDEWERKER_WIJZIG, ingelogdeOrganisatieMedewerker, melding);
	}

	private boolean isRolGewijzigd(OrganisatieMedewerkerRolDto initieleRol, String initieleBevolkingsonderzoeken, OrganisatieMedewerkerRol rol, String huidigeBevolkingsonderzoeken)
	{
		return !huidigeBevolkingsonderzoeken.equals(initieleBevolkingsonderzoeken) || !Objects.equals(rol.getBeginDatum(), initieleRol.getBeginDatum())
			|| !Objects.equals(rol.getEindDatum(), initieleRol.getEindDatum()) || rol.getActief() != initieleRol.getActief();
	}

	private String maakRolDatumString(Date date)
	{
		return date == null ? "(leeg)" : DateUtil.formatShortDate(date);
	}

	@Override
	@Transactional
	public boolean saveOrUpdateMedewerker(Medewerker medewerker, boolean isBestaande, boolean wordGeblokkeerd)
	{
		var gelukt = true;
		var handtekening = medewerker.getHandtekening();
		if (handtekening != null && !handtekening.getActief())
		{
			medewerker.setHandtekening(null);
			if (handtekening.getId() != null)
			{
				uploadDocumentService.delete(handtekening);
			}
		}
		if (isBestaande)
		{
			hibernateService.saveOrUpdate(medewerker);
			if (wordGeblokkeerd)
			{
				authenticatieService.accountGeblokkeerd(medewerker);
			}
		}
		else
		{
			if (medewerker.getInlogMethode().equals(InlogMethode.UZIPAS))
			{
				hibernateService.saveOrUpdate(medewerker);
				if (medewerker.getUzinummer() != null)
				{
					authenticatieService.sendUziEmail(medewerker);
				}
			}
			else
			{
				gelukt = resetWachtwoord(medewerker);
			}
		}
		return gelukt;

	}

	@Override
	@Transactional
	public boolean resetWachtwoord(Medewerker medewerker)
	{
		medewerker.setWachtwoord(null);
		hibernateService.saveOrUpdate(medewerker);

		var geresetMedewerker = authenticatieService.requestNewPassword(medewerker.getGebruikersnaam(), medewerker.getEmailextra());

		return geresetMedewerker != null;
	}

	@Override
	public List<Medewerker> getActieveMedewerkersMetRecht(Recht recht)
	{
		var spec = MedewerkerSpecification.isActief()
			.and(PermissieSpecification.heeftRecht(recht).with(permissieJoin())
				.and(OrganisatieMedewerkerSpecification.filterActief(true)).with(organisatieMedewerkerJoin()));
		return medewerkerRepository.findWith(spec, FluentJpaQuery::distinct).fetch(g -> g.addSubgraph(Medewerker_.yubiKey)).all();
	}

	@Override
	public List<OrganisatieMedewerkerRol> getOrganisatieMedewerkersMetRolEnBvos(Rol rol, List<Bevolkingsonderzoek> onderzoeken)
	{
		var rollen = new ArrayList<OrganisatieMedewerkerRol>();
		var organisatieMedewerkersRollen = getOrganisatieMedewerkersMetRol(rol);
		if (onderzoeken == null)
		{
			rollen.addAll(organisatieMedewerkersRollen);
		}
		else
		{
			rollen = organisatieMedewerkersRollen.stream()
				.filter(omRol -> onderzoeken.stream().anyMatch(verwijderdeOnderzoek -> omRol.getBevolkingsonderzoeken().contains(verwijderdeOnderzoek)))
				.collect(Collectors.toCollection(ArrayList::new));
		}
		return rollen;
	}

	@Override
	public OrganisatieMedewerker getOrganisatieMedewerker(Organisatie organisatie, Medewerker medewerker)
	{
		var spec = HibernateObjectSpecification.heeftId(organisatie.getId()).with(OrganisatieMedewerker_.organisatie)
			.and(HibernateObjectSpecification.heeftId(medewerker.getId()).with(OrganisatieMedewerker_.medewerker));
		return organisatieMedewerkerRepository.findOne(spec).orElse(null);
	}

	@Override
	public List<OrganisatieMedewerkerRol> getOrganisatieMedewerkersMetRol(Rol rol)
	{
		var spec = OrganisatieMedewerkerRolSpecification.isActief(Boolean.TRUE)
			.and(OrganisatieMedewerkerRolSpecification.heeftRol(rol));

		return organisatieMedewerkerRolRepository.findWith(spec, q -> q)
			.fetch(g -> g.addAttributeNodes(OrganisatieMedewerkerRol_.BEVOLKINGSONDERZOEKEN))
			.all();
	}

	@Override
	public boolean zijnErOrganisatieMedewerkersMetRol(Rol rol)
	{
		return !getOrganisatieMedewerkersMetRolEnBvos(rol, null).isEmpty();
	}

	public Specification<OrganisatieMedewerker> getSpecificationVoorOrganisatieMedewerker(OrganisatieMedewerker zoekOrganisatieMedewerker)
	{
		var spec = filterActief(zoekOrganisatieMedewerker.getActief());

		if (zoekOrganisatieMedewerker.getOrganisatie() != null)
		{
			if (zoekOrganisatieMedewerker.getOrganisatie().getId() != null)
			{
				spec = spec.and(HibernateObjectSpecification.heeftId(zoekOrganisatieMedewerker.getOrganisatie().getId()).with(OrganisatieMedewerker_.organisatie));
			}
			else
			{
				spec = spec.and(HibernateObjectSpecification.heeftGeenId().with(OrganisatieMedewerker_.organisatie));
			}
		}

		var medewerker = zoekOrganisatieMedewerker.getMedewerker();

		if (medewerker != null)
		{
			spec = spec.and(MedewerkerSpecification.filterActief(medewerker.getActief())
				.and(MedewerkerSpecification.filterActiefVanaf(medewerker.getActiefVanaf())
					.and(MedewerkerSpecification.filterActiefTotEnMet(medewerker.getActiefTotEnMet())
						.and(HibernateObjectSpecification.filterId(medewerker.getId())))).with(OrganisatieMedewerker_.medewerker));
		}

		return spec;
	}

	private Function<From<?, ? extends OrganisatieMedewerker>, From<?, ? extends Permissie>> permissieJoin()
	{
		return r ->
		{
			var rollenJoin = join(r, OrganisatieMedewerker_.rollen);
			var rolJoin = join(rollenJoin, OrganisatieMedewerkerRol_.rol);
			return join(rolJoin, Rol_.permissies);
		};
	}

	private Function<From<?, ? extends Medewerker>, From<?, ? extends OrganisatieMedewerker>> organisatieMedewerkerJoin()
	{
		return r ->
			join(r, Medewerker_.organisatieMedewerkers);
	}

}
