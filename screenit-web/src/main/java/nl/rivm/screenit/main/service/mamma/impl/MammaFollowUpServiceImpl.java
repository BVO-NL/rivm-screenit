package nl.rivm.screenit.main.service.mamma.impl;

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

import java.time.LocalDate;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.MammaFollowUpOrganisatieDto;
import nl.rivm.screenit.dto.mamma.MammaFollowUpOrganisatieRadiologieDto;
import nl.rivm.screenit.main.model.mamma.MammaFollowUpConclusieChoice;
import nl.rivm.screenit.main.service.mamma.MammaConclusieReviewService;
import nl.rivm.screenit.main.service.mamma.MammaFollowUpService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.Organisatie_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ScreeningRondeStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag_;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag;
import nl.rivm.screenit.repository.mamma.MammaBaseFollowUpRepository;
import nl.rivm.screenit.repository.mamma.MammaDossierRepository;
import nl.rivm.screenit.service.BaseVerslagService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseFollowUpService;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.BeoordelingsEenheidSpecification;
import nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_GUNSTIG;
import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.filterOnderzoekCreatieDatumAangemaaktTussen;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.filterOpBeoordelingStatus;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.filterOpScreeningOrganisatie;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.heeftAangemaaktOpOfVoor;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.heeftGeenFollowUpConclusieStatus;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.heeftGeenIngevoerdDoor;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.heeftLaatsteBeoordelingMetUitslag;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.heeftRondeZonderPaVerslag;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.heeftScreeningRondeGelijkMetUitnodiging;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.isAangemaaktInAfdeling;
import static nl.rivm.screenit.specification.mamma.MammaFollowUpRadiologieVerslagSpecification.isPaNogTeVerwachtenEnPathologieUitgevoerd;
import static org.springframework.data.jpa.domain.Specification.not;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaFollowUpServiceImpl implements MammaFollowUpService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaBaseFollowUpService baseFollowUpService;

	@Autowired
	private MammaConclusieReviewService conclusieReviewService;

	@Autowired
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	@Autowired
	private MammaDossierRepository dossierRepository;

	@Autowired
	private BaseVerslagService verslagService;

	@Autowired
	private MammaBaseFollowUpRepository followUpRepository;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveOrUpdateRadiologie(MammaFollowUpRadiologieVerslag verslag, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		MammaScreeningRonde screeningRonde = verslag.getScreeningRonde();
		verslag.setIngevoerdDoor(ingelogdeOrganisatieMedewerker);
		verslag.setIngevoerdOp(dateSupplier.getDate());
		verslag.setScreeningRonde(screeningRonde);
		screeningRonde.getFollowUpRadiologieVerslagen().add(verslag);
		hibernateService.saveOrUpdateAll(verslag, screeningRonde);
		baseFollowUpService.refreshUpdateFollowUpConclusie(screeningRonde.getDossier());

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_RADIOLOGIE_VERSLAG_OPGESLAGEN, ingelogdeOrganisatieMedewerker, screeningRonde.getDossier().getClient(),
			Bevolkingsonderzoek.MAMMA);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void saveFollowUpConclusieStatus(MammaScreeningRonde screeningRonde, MammaFollowUpConclusieStatus followUpConclusieStatus,
		Account ingelogdeOrganisatieMedewerker)
	{
		Date nu = dateSupplier.getDate();

		screeningRonde.setFollowUpConclusieStatus(followUpConclusieStatus);
		screeningRonde.setFollowUpConclusieStatusGewijzigdOp(nu);
		screeningRonde.setStatus(ScreeningRondeStatus.AFGEROND);
		screeningRonde.setStatusDatum(nu);
		hibernateService.saveOrUpdate(screeningRonde);
		baseFollowUpService.refreshUpdateFollowUpConclusie(screeningRonde.getDossier());

		conclusieReviewService.maakConclusieReviewVoorBetrokkenRadiologen(screeningRonde);

		volgendeUitnodigingService.updateVolgendeUitnodigingNaFollowUpConclusie(screeningRonde);

		verwijderElectronischePalgaVerslagen(screeningRonde);

		logService.logGebeurtenis(LogGebeurtenis.MAMMA_FOLLOW_UP_CONCLUSIE, ingelogdeOrganisatieMedewerker, screeningRonde.getDossier().getClient(),
			"Conclusie: " + followUpConclusieStatus, Bevolkingsonderzoek.MAMMA);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void savePaVerslagNietTeVerwachten(MammaFollowUpRadiologieVerslag followUpRadiologieVerslag, Account ingelogdeOrganisatieMedewerker)
	{
		followUpRadiologieVerslag.setPaVerslagNietTeVerwachten(dateSupplier.getDate());
		hibernateService.saveOrUpdate(followUpRadiologieVerslag);
		baseFollowUpService.refreshUpdateFollowUpConclusie(followUpRadiologieVerslag.getScreeningRonde().getDossier());
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_FOLLOW_UP_PA_NIET_TE_VERWACHTEN, ingelogdeOrganisatieMedewerker,
			followUpRadiologieVerslag.getScreeningRonde().getDossier().getClient(), Bevolkingsonderzoek.MAMMA);
	}

	@Override
	public MammaFollowUpConclusieStatus bepaalFollowUpConclusie(MammaScreeningRonde screeningRonde, MammaFollowUpConclusieChoice conclusieEnum)
	{
		MammaBeoordelingStatus laatsteBeoordelingStatus = screeningRonde.getLaatsteUitnodiging().getLaatsteAfspraak().getOnderzoek()
			.getLaatsteBeoordeling().getStatus();
		MammaFollowUpConclusieStatus conclusieStatus = null;

		switch (laatsteBeoordelingStatus)
		{
		case ONBEOORDEELBAAR:
		case UITSLAG_GUNSTIG:
			if (conclusieEnum.equals(MammaFollowUpConclusieChoice.NEGATIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.TRUE_NEGATIVE;
			}
			else if (conclusieEnum.equals(MammaFollowUpConclusieChoice.POSITIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.FALSE_NEGATIVE;
			}
			break;
		case UITSLAG_ONGUNSTIG:
			if (conclusieEnum.equals(MammaFollowUpConclusieChoice.NEGATIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.FALSE_POSITIVE;
			}
			else if (conclusieEnum.equals(MammaFollowUpConclusieChoice.POSITIEF))
			{
				conclusieStatus = MammaFollowUpConclusieStatus.TRUE_POSITIVE;
			}
			break;
		}

		if (conclusieEnum.equals(MammaFollowUpConclusieChoice.NIET_TE_VERWACHTEN))
		{
			conclusieStatus = MammaFollowUpConclusieStatus.NIET_TE_VERWACHTEN;
		}

		return conclusieStatus;
	}

	@Override
	public List<MammaFollowUpRadiologieVerslag> getIngevoerdeFollowUpRadiologieVerslagen(MammaScreeningRonde screeningRonde)
	{
		return screeningRonde
			.getFollowUpRadiologieVerslagen().stream()
			.filter(v -> v.getIngevoerdOp() != null)
			.sorted(Comparator.comparing(MammaFollowUpRadiologieVerslag::getIngevoerdOp, Comparator.reverseOrder()))
			.collect(Collectors.toList());
	}

	@Override
	public List<MammaFollowUpVerslag> getAfgerondeFollowUpPathologieVerslagen(MammaScreeningRonde screeningRonde)
	{
		return baseFollowUpService.getFollowUpVerslagenZonderLandelijkeMonitor(screeningRonde).stream()
			.filter(v -> v.getStatus().equals(VerslagStatus.AFGEROND))
			.sorted(Comparator.comparing(MammaVerslag::getDatumVerwerkt, Comparator.reverseOrder()))
			.collect(Collectors.toList());
	}

	@Override
	public List<MammaBeoordeling> zoekOpenstaandeFollowUpConclusies(ScreeningOrganisatie screeningorganisatie, long first, long count, Sort sort)
	{
		return dossierRepository.findWith(
			openstaandeFollowupConclusiesSpecification(screeningorganisatie),
			MammaBeoordeling.class,
			q -> q.projection((cb, r) -> r.get(MammaDossier_.laatsteBeoordelingMetUitslag)).sortBy(sort).all(first, count));
	}

	@Override
	public long countOpenstaandeFollowUpConclusies(ScreeningOrganisatie screeningorganisatie)
	{
		return dossierRepository.count(openstaandeFollowupConclusiesSpecification(screeningorganisatie));
	}

	@Override
	public List<MammaFollowUpRadiologieVerslag> zoekDossiersMetOpenstaandePaVerslagen(Organisatie organisatie, long first, long count, Sort sort)
	{
		return followUpRepository.findWith(openstaandePaVerslagenVoorZiekenhuisafdeling(organisatie), q -> q.sortBy(sort).all(first, count));
	}

	@Override
	public long countDossiersMetOpenstaandePaVerslagen(Organisatie organisatie)
	{
		return followUpRepository.count(openstaandePaVerslagenVoorZiekenhuisafdeling(organisatie));
	}

	@Override
	public List<MammaFollowUpOrganisatieDto> zoekOrganisatiesMetOpenstaandePaVerslagen(ScreeningOrganisatie regio)
	{
		return followUpRepository.findWith(openstaandePaVerslagenVoorScreeningsorganisatie(regio),
			MammaFollowUpOrganisatieDto.class,
			q ->
				q.projections((cb, r) ->
					{
						var organisatieJoin = join(r, MammaFollowUpRadiologieVerslag_.aangemaaktIn);
						return List.of(
							organisatieJoin.get(AbstractHibernateObject_.id),
							organisatieJoin.get(Organisatie_.naam),
							cb.least(r.get(MammaFollowUpRadiologieVerslag_.laatstGebeldOverPaVerslag)),
							organisatieJoin.get(Organisatie_.telefoon),
							organisatieJoin.get(Organisatie_.telefoon2)
						);
					}
				).groupBy((cb, r) ->
				{
					var organisatieJoin = join(r, MammaFollowUpRadiologieVerslag_.aangemaaktIn);
					return List.of(
						organisatieJoin.get(AbstractHibernateObject_.id),
						organisatieJoin.get(Organisatie_.naam),
						organisatieJoin.get(Organisatie_.telefoon),
						organisatieJoin.get(Organisatie_.telefoon2)
					);
				}).all());
	}

	private static Specification<MammaFollowUpRadiologieVerslag> openstaandePaVerslagenVoorScreeningsorganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return openstaandePaVerslagen().and(filterOpScreeningOrganisatie(screeningOrganisatie));
	}

	private static Specification<MammaFollowUpRadiologieVerslag> openstaandePaVerslagenVoorZiekenhuisafdeling(Organisatie afdeling)
	{
		return openstaandePaVerslagen().and(isAangemaaktInAfdeling(afdeling));
	}

	private static Specification<MammaFollowUpRadiologieVerslag> openstaandePaVerslagen()
	{
		return heeftScreeningRondeGelijkMetUitnodiging()
			.and(heeftGeenFollowUpConclusieStatus())
			.and(isPaNogTeVerwachtenEnPathologieUitgevoerd())
			.and(heeftRondeZonderPaVerslag());
	}

	@Override
	public List<MammaFollowUpOrganisatieRadiologieDto> zoekOpenstaandeRadiologieVerslagenPerOrganisatie(ScreeningOrganisatie regio,
		MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie, Integer jaar)
	{
		var aangemaaktOp = DateUtil.toUtilDate(
			dateSupplier.getLocalDate().minusDays(preferenceService.getInteger(PreferenceKey.MAMMA_FOLLOW_UP_RADIOLOGIE_WERKLIJST_NA_DOWNLOADEN.name())));
		return followUpRepository.findWith(beoordelingStatusFilter(doorverwezenFilterOptie)
				.and(heeftLaatsteBeoordelingMetUitslag())
				.and(heeftGeenIngevoerdDoor())
				.and(heeftAangemaaktOpOfVoor(aangemaaktOp))
				.and(filterOpScreeningOrganisatie(regio))
				.and(onderZoekDatum(jaar)),
			MammaFollowUpOrganisatieRadiologieDto.class,
			q ->
				q.projections((cb, r) ->
					{
						var organisatieJoin = join(r, MammaFollowUpRadiologieVerslag_.aangemaaktIn);
						return List.of(
							organisatieJoin.get(AbstractHibernateObject_.id),
							organisatieJoin.get(Organisatie_.naam),
							organisatieJoin.get(Organisatie_.mammaRadiologieGebeld),
							cb.count(r.get(AbstractHibernateObject_.id)),
							organisatieJoin.get(Organisatie_.telefoon),
							organisatieJoin.get(Organisatie_.telefoon2)
						);
					}
				).groupBy((cb, r) ->
				{
					var organisatieJoin = join(r, MammaFollowUpRadiologieVerslag_.aangemaaktIn);
					return List.of(
						organisatieJoin.get(AbstractHibernateObject_.id),
						organisatieJoin.get(Organisatie_.naam),
						organisatieJoin.get(Organisatie_.mammaRadiologieGebeld),
						organisatieJoin.get(Organisatie_.telefoon),
						organisatieJoin.get(Organisatie_.telefoon2)
					);
				}).all());
	}

	private Specification<MammaFollowUpRadiologieVerslag> beoordelingStatusFilter(MammaFollowUpDoorverwezenFilterOptie doorverwezenFilterOptie)
	{
		return switch (doorverwezenFilterOptie)
		{
			case DOORVERWEZEN -> filterOpBeoordelingStatus(UITSLAG_ONGUNSTIG);
			case NIET_DOORVERWEZEN -> filterOpBeoordelingStatus(UITSLAG_GUNSTIG);
			case ALLES -> filterOpBeoordelingStatus(null);
		};
	}

	private Specification<MammaFollowUpRadiologieVerslag> onderZoekDatum(Integer jaar)
	{
		if (jaar == null)
		{
			return filterOnderzoekCreatieDatumAangemaaktTussen(null);
		}
		var vanaf = DateUtil.toUtilDate(LocalDate.of(jaar, 1, 1));
		var tot = DateUtil.toUtilDate(LocalDate.of(jaar + 1, 1, 1));
		var range = Range.closedOpen(vanaf, tot);

		return filterOnderzoekCreatieDatumAangemaaktTussen(range);
	}

	private Specification<MammaDossier> openstaandeFollowupConclusiesSpecification(ScreeningOrganisatie screeningorganisatie)
	{
		return MammaBaseDossierSpecification.isUpdateFollowUpConclusie(true)
			.and(filterOpLaatsteBeoordelingVoorScreeningorganisatie(screeningorganisatie))
			.and(not(HibernateObjectSpecification.heeftGeenId().with(MammaDossier_.laatsteBeoordelingMetUitslag)));
	}

	private Specification<MammaDossier> filterOpLaatsteBeoordelingVoorScreeningorganisatie(ScreeningOrganisatie screeningorganisatie)
	{
		return skipWhenNullExtended(screeningorganisatie, BeoordelingsEenheidSpecification.filterOpScreeningOrganisatie(screeningorganisatie).with(r ->
		{
			var beoordelingJoin = join(r, MammaDossier_.laatsteBeoordelingMetUitslag);
			return join(beoordelingJoin, MammaBeoordeling_.beoordelingsEenheid);
		}));
	}

	private void verwijderElectronischePalgaVerslagen(MammaScreeningRonde screeningRonde)
	{
		if (MammaFollowUpConclusieStatus.TRUE_NEGATIVE == screeningRonde.getFollowUpConclusieStatus())
		{
			getAfgerondeFollowUpPathologieVerslagen(screeningRonde).stream()
				.filter(verslagService::isDigitaalPalgaVerslag)
				.forEach(verslag -> verslagService.verwijderVerslag(verslag, null, false));
		}
	}
}
