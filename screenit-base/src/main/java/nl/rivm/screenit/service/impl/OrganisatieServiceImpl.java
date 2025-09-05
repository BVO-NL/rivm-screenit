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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import jakarta.annotation.Nonnull;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.JoinType;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dto.mamma.planning.PlanningScreeningsOrganisatieDto;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol_;
import nl.rivm.screenit.model.OrganisatieMedewerker_;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.Organisatie_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColoscopieLocatie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.repository.algemeen.BeoordelingsEenheidRepository;
import nl.rivm.screenit.repository.algemeen.CentraleEenheidRepository;
import nl.rivm.screenit.repository.algemeen.OrganisatieMedewerkerRepository;
import nl.rivm.screenit.repository.algemeen.OrganisatieRepository;
import nl.rivm.screenit.repository.algemeen.ScreeningOrganisatieRepository;
import nl.rivm.screenit.repository.colon.ColonIntakelocatieRepository;
import nl.rivm.screenit.service.CoordinatenService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.OrganisatieService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerRolSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.specification.algemeen.OrganisatieSpecification;
import nl.rivm.screenit.specification.algemeen.RolSpecification;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.commons.lang3.BooleanUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftColoscopielocatieId;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftColoscopielocatieParent;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftOrganisatieType;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftOrganisatieTypes;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftRootOid;
import static nl.rivm.screenit.specification.algemeen.OrganisatieSpecification.heeftUziAbonneenummer;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

@Slf4j
@Service
public class OrganisatieServiceImpl implements OrganisatieService
{

	private static final String GEEN_WAARDE = "(geen waarde)";

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private CoordinatenService coordinatenService;

	@Autowired
	private UploadDocumentService uploadDocumentService;

	@Autowired
	private LogService logService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired(required = false)
	private MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	@Autowired
	private ScreeningOrganisatieRepository screeningOrganisatieRepository;

	@Autowired
	private OrganisatieRepository organisatieRepository;

	@Autowired
	private ColonIntakelocatieRepository colonIntakelocatieRepository;

	@Autowired
	private OrganisatieMedewerkerRepository organisatieMedewerkerRepository;

	@Autowired
	private CentraleEenheidRepository centraleEenheidRepository;

	@Autowired
	private BeoordelingsEenheidRepository beoordelingEenheidRepository;

	@Override
	public List<CentraleEenheid> getMogelijkeCentraleEenheden(Organisatie organisatie)
	{
		if (organisatie == null)
		{
			return Collections.emptyList();
		}
		OrganisatieType organisatieType = organisatie.getOrganisatieType();
		return switch (organisatieType)
		{
			case RIVM, KWALITEITSPLATFORM -> getActieveOrganisaties(CentraleEenheid.class);
			case BEOORDELINGSEENHEID -> Collections.singletonList((CentraleEenheid) hibernateService.deproxy(organisatie.getParent()));
			case SCREENINGSORGANISATIE -> getActieveCentraleEenhedenBinnenRegio((ScreeningOrganisatie) hibernateService.deproxy(organisatie));
			default -> Collections.emptyList();
		};
	}

	@Override
	public List<OrganisatieMedewerker> getActieveOrganisatieMedewerkers(@NotNull Medewerker medewerker)
	{
		return organisatieMedewerkerRepository.findAll(OrganisatieMedewerkerSpecification.isActief()
			.and(OrganisatieMedewerkerSpecification.heeftMedewerker(medewerker))
			.and(OrganisatieSpecification.isActief(true).with(OrganisatieMedewerker_.organisatie)));
	}

	@Override
	public List<OrganisatieMedewerker> getActieveOrganisatieMedewerkersMetRollen(Medewerker medewerker)
	{
		var organisatieMedewerkers = organisatieMedewerkerRepository.findAll(OrganisatieMedewerkerSpecification.isActief()
				.and(OrganisatieMedewerkerSpecification.heeftMedewerker(medewerker))
				.and(OrganisatieSpecification.isActief(true).with(OrganisatieMedewerker_.organisatie))
				.and(OrganisatieSpecification.heeftOrganisatieType(OrganisatieType.HUISARTS).with(OrganisatieMedewerker_.organisatie)
					.or(OrganisatieMedewerkerRolSpecification.isActiefOpDatum(currentDateSupplier.getLocalDate()).with(organisatieMedewerkerRolJoin()).and(
						RolSpecification.isActief(true).with(r -> join(join(r, OrganisatieMedewerker_.rollen, JoinType.LEFT), OrganisatieMedewerkerRol_.rol, JoinType.LEFT))))),
			Sort.by(Sort.Order.asc(propertyChain(OrganisatieMedewerker_.ORGANISATIE, Organisatie_.NAAM))));

		return organisatieMedewerkers.stream().distinct().toList();
	}

	private static Function<From<?, ? extends OrganisatieMedewerker>, From<?, ? extends OrganisatieMedewerkerRol>> organisatieMedewerkerRolJoin()
	{
		return r -> join(r, OrganisatieMedewerker_.rollen, JoinType.LEFT);
	}

	@Override
	public List<ColonIntakelocatie> getActieveIntakelocaties()
	{
		return getActieveOrganisaties(ColonIntakelocatie.class);
	}

	@Override
	public List<ColonIntakelocatie> getActieveIntakelocatiesBinnenRegio(ScreeningOrganisatie regio)
	{
		var specification = OrganisatieSpecification.<ColonIntakelocatie> isActieveOrganisatie(ColonIntakelocatie.class).and(OrganisatieSpecification.heeftRegio(regio));
		return colonIntakelocatieRepository.findAll(specification, Sort.by(Sort.Order.asc(Organisatie_.NAAM)));
	}

	@Override
	public List<BeoordelingsEenheid> getActieveBeoordelingseenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		var specification = OrganisatieSpecification.<BeoordelingsEenheid> isActieveOrganisatie(BeoordelingsEenheid.class)
			.and(BeoordelingsEenheidSpecification.heeftScreeningOrganisatie(regio));
		return beoordelingEenheidRepository.findAll(specification, Sort.by(Sort.Order.asc(Organisatie_.NAAM)));
	}

	@Override
	public List<CentraleEenheid> getActieveCentraleEenhedenBinnenRegio(ScreeningOrganisatie regio)
	{
		var specification = OrganisatieSpecification.<CentraleEenheid> isActieveOrganisatie(CentraleEenheid.class).and(OrganisatieSpecification.heeftRegio(regio));
		return centraleEenheidRepository.findAll(specification, Sort.by(Sort.Order.asc(Organisatie_.NAAM)));
	}

	@Override
	@Transactional
	public void saveOrUpdateScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie, List<Gemeente> choices, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		List<Gemeente> gekoppeldeGemeentes = screeningOrganisatie.getGemeentes();
		for (Gemeente gemeente : gekoppeldeGemeentes)
		{
			gemeente.setScreeningOrganisatie(screeningOrganisatie);
			hibernateService.saveOrUpdate(gemeente);
		}
		for (Gemeente gemeente : choices)
		{
			if (!gekoppeldeGemeentes.contains(gemeente) && screeningOrganisatie.equals(gemeente.getScreeningOrganisatie()))
			{

				gemeente.setScreeningOrganisatie(null);
				hibernateService.saveOrUpdate(gemeente);
			}
		}

		for (ZASRetouradres retouradres : screeningOrganisatie.getRetouradressen())
		{
			hibernateService.saveOrUpdateAll(retouradres.getAdres(), retouradres);
		}

		hibernateService.saveOrUpdate(screeningOrganisatie);

	}

	@Override
	@Transactional
	public void saveOrUpdateSoPlanningBk(ScreeningOrganisatie screeningOrganisatie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		PlanningScreeningsOrganisatieDto screeningsOrganisatieDto = new PlanningScreeningsOrganisatieDto();
		screeningsOrganisatieDto.id = screeningOrganisatie.getId();
		screeningsOrganisatieDto.factorMinderValideBk = screeningOrganisatie.getFactorMinderValideBk();
		screeningsOrganisatieDto.factorDubbeleTijdBk = screeningOrganisatie.getFactorDubbeleTijdBk();
		screeningsOrganisatieDto.factorEersteOnderzoekBk = screeningOrganisatie.getFactorEersteOnderzoekBk();
		screeningsOrganisatieDto.wekenVanTevorenUitnodigen = screeningOrganisatie.getWekenVanTevorenUitnodigen();
		screeningsOrganisatieDto.vervallenCapaciteitsreserveringDagenBk = screeningOrganisatie.getVervallenCapaciteitsreserveringDagenBk();
		screeningsOrganisatieDto.minimaleDagCapaciteitMinderValideAfspraken = screeningOrganisatie.getMinimaleDagCapaciteitMinderValideAfspraken();
		baseConceptPlanningsApplicatie.updateScreeningsOrganisatie(screeningsOrganisatieDto);

		String oudeAfspraakDrempelBk = EntityAuditUtil.getDiffFieldsToLatestVersion(screeningOrganisatie, hibernateService.getHibernateSession(), "afspraakDrempelBk");
		if (!oudeAfspraakDrempelBk.equals(""))
		{
			oudeAfspraakDrempelBk = oudeAfspraakDrempelBk.split(" -> ")[0].split(": ")[1];
			if (!GEEN_WAARDE.equals(oudeAfspraakDrempelBk))
			{
				oudeAfspraakDrempelBk += "%";
			}

			String nieuweAfspraakDrempelBk;
			if (screeningOrganisatie.getAfspraakDrempelBk() != null)
			{
				nieuweAfspraakDrempelBk = screeningOrganisatie.getAfspraakDrempelBk().toString() + "%";
			}
			else
			{
				nieuweAfspraakDrempelBk = GEEN_WAARDE;
			}

			if (!oudeAfspraakDrempelBk.equals(nieuweAfspraakDrempelBk))
			{
				String logMeldingAfspraakDrempelBk = "De afspraakdrempel is voor " + screeningOrganisatie.getNaam() + " gezet van "
					+ oudeAfspraakDrempelBk + " naar " + nieuweAfspraakDrempelBk + ".";
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_AFSPRAAK_DREMPEL_GEWIJZIGD, ingelogdeOrganisatieMedewerker, logMeldingAfspraakDrempelBk, Bevolkingsonderzoek.MAMMA);
			}
		}
		hibernateService.saveOrUpdate(screeningOrganisatie);
	}

	@Override
	@Transactional
	public void saveOrUpdate(Organisatie organisatie)
	{
		hibernateService.saveOrUpdate(organisatie);
		if (organisatie instanceof ColonIntakelocatie intakelocatie)
		{

			var organisatieParameterService = ApplicationContextProvider.getApplicationContext().getBean(OrganisatieParameterService.class);
			var duurAfspraakInMinutenParameter = organisatieParameterService.getParameter(intakelocatie, OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN);

			if (duurAfspraakInMinutenParameter == null)
			{
				organisatieParameterService.maakOfUpdateOrganisatieParameter(OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN, "15", intakelocatie);
			}
			var coordinaten = coordinatenService.getCoordinaten(organisatie);
			intakelocatie.setPostcodeCoordinaten(coordinaten);
		}
		hibernateService.saveOrUpdate(organisatie);
	}

	@Override
	@Transactional
	public void saveOrUpdateColoscopieCentrum(ColonIntakelocatie intakelocatie)
	{
		hibernateService.saveOrUpdate(intakelocatie);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends Organisatie> List<T> getActieveOrganisaties(Class<T> organisatieClass)
	{
		return organisatieRepository
			.findAll(OrganisatieSpecification.isActieveOrganisatie(organisatieClass), Sort.by(Sort.Order.asc(Organisatie_.NAAM)))
			.stream()
			.map(o -> (T) hibernateService.deproxy(o))
			.collect(Collectors.toList());
	}

	@Override
	public List<ScreeningOrganisatie> getAllActiefScreeningOrganisaties()
	{
		return getActieveOrganisaties(ScreeningOrganisatie.class);
	}

	@Override
	public Organisatie getOrganisatieByUzinummer(String uzinummer)
	{
		return organisatieRepository.findOne(heeftUziAbonneenummer(uzinummer).and(OrganisatieSpecification.isActief(true))).orElse(null);
	}

	@Override
	public Organisatie getOrganisatieByRootOid(String rootOid)
	{
		return organisatieRepository.findOne(heeftRootOid(rootOid).and(OrganisatieSpecification.isActief(true))).orElse(null);
	}

	@Override
	public List<Organisatie> getOrganisatieByOrganisatieTypes(List<OrganisatieType> organisatieTypes)
	{
		return organisatieRepository.findAll(heeftOrganisatieTypes(organisatieTypes).and(OrganisatieSpecification.isActief(true)),
			Sort.by(Sort.Order.asc(Organisatie_.NAAM)));
	}

	@Override
	public List<Medewerker> getActieveMedewerkers(Organisatie organisatie)
	{
		List<Medewerker> medewerkers = new ArrayList<>();

		if (organisatie.getOrganisatieMedewerkers() != null)
		{
			for (OrganisatieMedewerker organisatieMedewerker : organisatie.getOrganisatieMedewerkers())
			{
				final Medewerker medewerker = organisatieMedewerker.getMedewerker();
				if (BooleanUtils.isNotFalse(organisatieMedewerker.getActief()) && MedewerkerUtil.isMedewerkerActief(medewerker, currentDateSupplier.getDate())
					&& !medewerkers.contains(medewerker))
				{
					medewerkers.add(medewerker);
				}
			}
		}

		return medewerkers;
	}

	@Override
	public List<Organisatie> getPathologieLabs(@NotNull Organisatie organisatie)
	{
		var specification = OrganisatieSpecification.isActief(true);
		if (organisatie instanceof ColoscopieLocatie)
		{
			specification = specification.and(heeftColoscopielocatieId(organisatie.getId()));
		}
		else
		{
			specification = specification.and(heeftColoscopielocatieParent(organisatie.getId()));
		}
		return organisatieRepository.findAll(specification, Sort.by(Sort.Order.asc(Organisatie_.NAAM)));
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends Organisatie> List<T> getChildrenOrganisaties(@Nonnull Organisatie organisatie, @Nonnull Class<T> organisatieClass)
	{
		var specification = OrganisatieSpecification.isActief(true).and(OrganisatieSpecification.heeftParent(organisatie, organisatieClass));

		return organisatieRepository
			.findAll(specification, Sort.by(Sort.Order.asc(Organisatie_.NAAM)))
			.stream()
			.map(o -> (T) hibernateService.deproxy(o))
			.collect(Collectors.toList());
	}

	@Override
	@Transactional
	public void saveDocumentForOrganisatie(UploadDocument uploadDocument, Organisatie organisatie)
	{
		List<UploadDocument> documents = organisatie.getDocuments();
		try
		{
			uploadDocumentService.saveOrUpdate(uploadDocument, FileStoreLocation.ORGANISATIE_DOCUMENTEN, organisatie.getId());
			documents.add(uploadDocument);
			organisatie.setDocuments(documents);
			hibernateService.saveOrUpdate(organisatie);
		}
		catch (IOException e)
		{
			LOG.error("Er is een fout opgetreden! {}", e.getMessage(), e);
		}
	}

	@Override
	@Transactional
	public void deleteDocumentForOrganisatie(UploadDocument document, Organisatie organisatie)
	{
		uploadDocumentService.deleteDocumentFromList(document, organisatie.getDocuments());
	}

	@Override
	public ScreeningOrganisatie getScreeningOrganisatie(long screeningOrganisatieId)
	{
		return screeningOrganisatieRepository.findById(screeningOrganisatieId).orElseThrow();
	}

	@Override
	public List<Long> getOrganisatieIdsMetType(OrganisatieType type)
	{
		return organisatieRepository.findWith(heeftOrganisatieType(type).and(OrganisatieSpecification.isActief(true)), Long.class,
			q -> q.projection((cb, r) -> r.get(AbstractHibernateObject_.id))).all();
	}

}
