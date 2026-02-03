package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakOptieMetAfstandDto;
import nl.rivm.screenit.dto.mamma.planning.PlanningVerzetClientenDto;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.transformer.MammaScreeningsEenheidMetDatumDto;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBlokkade;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.repository.mamma.MammaBaseAfspraakRepository;
import nl.rivm.screenit.repository.mamma.MammaUitnodigingRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BerichtToSeRestBkService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernateSessionInThread;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static java.time.temporal.ChronoUnit.DAYS;
import static nl.rivm.screenit.specification.SpecificationUtil.cast;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.heeftStatusIn;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.isLaatsteAfspraakVanUitnodiging;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.valtInDatumPeriode;
import static nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification.valtOnderBlokkadeType;
import static nl.rivm.screenit.specification.mamma.MammaUitnodigingSpecification.heeftLaatsteAfspraakMetStandplaatsPeriode;
import static nl.rivm.screenit.specification.mamma.MammaUitnodigingSpecification.heeftLaatsteAfspraakMetStatus;

@Service
@Transactional(propagation = Propagation.REQUIRED)
@RequiredArgsConstructor
public class MammaAfspraakServiceImpl implements MammaAfspraakService
{
	private final HibernateService hibernateService;

	private final MammaBaseConceptPlanningsApplicatie baseConceptPlanningsApplicatie;

	private final ICurrentDateSupplier dateSupplier;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final SimplePreferenceService preferenceService;

	private final MammaBaseStandplaatsService baseStandplaatsService;

	private final BerichtToSeRestBkService berichtToSeRestBkService;

	private final BaseBriefService baseBriefService;

	private final MammaUitnodigingRepository uitnodigingRepository;

	private final MammaBaseAfspraakRepository afspraakRepository;

	private static final int STREEF_INTERVAL = 2;

	private static final ExecutorService EXECUTOR_SERVICE = Executors.newSingleThreadExecutor();

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Map<MammaScreeningsEenheid, List<LocalDate>> getAfspraakDatums(MammaBlokkade blokkade)
	{
		var blokkadeRange = Range.closed(DateUtil.toLocalDate(blokkade.getVanaf()), DateUtil.toLocalDate(blokkade.getTotEnMet()));
		var afsprakenBinnenBlokkadeSpecification = valtInDatumPeriode(blokkadeRange)
			.and(heeftStatusIn(MammaAfspraakStatus.NIET_GEANNULEERD))
			.and(isLaatsteAfspraakVanUitnodiging())
			.and(valtOnderBlokkadeType(blokkade));
		var afspraakDatums = afspraakRepository.findWith(afsprakenBinnenBlokkadeSpecification, MammaScreeningsEenheidMetDatumDto.class,
			q -> q.projections((cb, r) ->
					List.of(
						join(r, MammaAfspraak_.standplaatsPeriode).get(MammaStandplaatsPeriode_.screeningsEenheid),
						cast(r.get(MammaAfspraak_.vanaf), LocalDate.class, cb)
					))
				.distinct()
				.sortBy(Sort.by(MammaAfspraak_.VANAF), (o, r, cb) -> cb.asc(cast(r.get(MammaAfspraak_.vanaf), LocalDate.class, cb)))
				.all());
		Map<MammaScreeningsEenheid, List<LocalDate>> screeningsEenheidAfspraakDatumsMap = new HashMap<>();
		afspraakDatums.forEach(afspraakDatum ->
		{
			if (screeningsEenheidAfspraakDatumsMap.containsKey(afspraakDatum.getScreeningsEenheid()))
			{
				screeningsEenheidAfspraakDatumsMap.get(afspraakDatum.getScreeningsEenheid()).add(afspraakDatum.getDatum());
			}
			else
			{
				List<LocalDate> datums = new ArrayList<>();
				datums.add(afspraakDatum.getDatum());
				screeningsEenheidAfspraakDatumsMap.put(afspraakDatum.getScreeningsEenheid(), datums);
			}
		});

		return screeningsEenheidAfspraakDatumsMap;
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Date getDatumEersteGeplandeAfspraak(Long standplaatsPeriodeId)
	{
		return getDatumVanAfspraakBinnenStandplaatsPeriodeMetSortOpVanaf(standplaatsPeriodeId, Sort.Direction.ASC);
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public Date getDatumLaatsteGeplandeAfspraak(Long standplaatsPeriodeId)
	{
		return getDatumVanAfspraakBinnenStandplaatsPeriodeMetSortOpVanaf(standplaatsPeriodeId, Sort.Direction.DESC);
	}

	private Date getDatumVanAfspraakBinnenStandplaatsPeriodeMetSortOpVanaf(Long standplaatsPeriodeId, Sort.Direction sortDirection)
	{
		return uitnodigingRepository.findWith(heeftLaatsteAfspraakMetStandplaatsPeriode(standplaatsPeriodeId)
				.and(heeftLaatsteAfspraakMetStatus(MammaAfspraakStatus.GEPLAND)),
			Date.class,
			q -> q.projection((cb, r) -> r.get(MammaUitnodiging_.laatsteAfspraak).get(MammaAfspraak_.vanaf))
				.sortBy(Sort.by(sortDirection, MammaUitnodiging_.LAATSTE_AFSPRAAK + "." + MammaAfspraak_.VANAF))
				.first()
		).orElse(null);
	}

	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	@Override
	public boolean kortVoorVolgendeRonde(MammaAfspraak afspraak)
	{
		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		MammaStandplaats huidigeStandplaats = baseStandplaatsService.getStandplaatsMetPostcode(dossier.getClient());

		MammaScreeningRonde laatsteScreeningRonde = dossier.getLaatsteScreeningRonde();

		if (laatsteScreeningRonde != null && huidigeStandplaats != null)
		{
			MammaStandplaatsRonde vorigeStandplaatsRondeClient = laatsteScreeningRonde.getStandplaatsRonde();
			LocalDate startMinimalePeriode;
			MammaStandplaatsPeriode eerstVolgendeStandplaatsPeriode = null;
			Date vandaag = dateSupplier.getDateMidnight();
			for (MammaStandplaatsRonde ronde : huidigeStandplaats.getStandplaatsRonden())
			{
				if (!ronde.equals(vorigeStandplaatsRondeClient))
				{
					for (MammaStandplaatsPeriode periode : ronde.getStandplaatsPerioden())
					{
						if (!periode.getTotEnMet().before(vandaag)
							&& (eerstVolgendeStandplaatsPeriode == null || eerstVolgendeStandplaatsPeriode.getVanaf().after(periode.getVanaf())))
						{
							eerstVolgendeStandplaatsPeriode = periode;
						}
					}
				}
			}
			if (eerstVolgendeStandplaatsPeriode != null)
			{
				if (huidigeStandplaats.equals(vorigeStandplaatsRondeClient.getStandplaats()))
				{
					int interval = eerstVolgendeStandplaatsPeriode.getStandplaatsRonde().getInterval() != null
						? eerstVolgendeStandplaatsPeriode.getStandplaatsRonde().getInterval().intValue()
						: STREEF_INTERVAL * 365;
					startMinimalePeriode = DateUtil.toLocalDate(laatsteScreeningRonde.getCreatieDatum())
						.plusDays(interval);
				}
				else
				{
					LocalDate vanaf = DateUtil.toLocalDate(eerstVolgendeStandplaatsPeriode.getVanaf());
					LocalDate totEnMet = DateUtil.toLocalDate(eerstVolgendeStandplaatsPeriode.getTotEnMet());
					startMinimalePeriode = vanaf.plusDays(DAYS.between(vanaf, totEnMet) / 2);
				}
				Integer minimaleIntervalVolgendeUitnodiging = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_UITNODIGINGEN.name());
				return startMinimalePeriode.minusDays(minimaleIntervalVolgendeUitnodiging).isBefore(DateUtil.toLocalDate(afspraak.getVanaf()));
			}
		}
		return false;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public String controleerAfspraakInAndereLocatie(MammaAfspraakOptieMetAfstandDto afspraakOptieDto, MammaDossier dossier)
	{
		MammaStandplaatsPeriode standplaatsPeriode = hibernateService.load(MammaStandplaatsPeriode.class, afspraakOptieDto.getStandplaatsPeriodeId());
		MammaAfspraak laatsteAfspraak = MammaScreeningRondeUtil.getLaatsteAfspraak(dossier.getLaatsteScreeningRonde());

		LocalDate nieuweAfspraakDatum = afspraakOptieDto.getDatum();
		if (laatsteAfspraak == null)
		{
			TijdelijkAdres tijdelijkeLocatie = standplaatsPeriode.getStandplaatsRonde().getStandplaats().getTijdelijkeLocatie();

			if (tijdelijkeLocatie.getStartDatum() != null &&
				!nieuweAfspraakDatum.isBefore(DateUtil.toLocalDate(tijdelijkeLocatie.getStartDatum()))
				&& !nieuweAfspraakDatum.isAfter(DateUtil.toLocalDate(tijdelijkeLocatie.getEindDatum())))
			{
				return "andere.locatie.tijdelijk";
			}
		}
		else
		{
			MammaStandplaats vorigeStandplaats = laatsteAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
			MammaStandplaats nieuweStandplaats = standplaatsPeriode.getStandplaatsRonde().getStandplaats();
			if (vorigeStandplaats.equals(nieuweStandplaats))
			{

				MammaStandplaatsLocatie tijdelijkeLocatie = standplaatsPeriode.getStandplaatsRonde().getStandplaats().getTijdelijkeLocatie();
				if (tijdelijkeLocatie.getStartDatum() != null)
				{
					boolean vorigeAfspraakInTijdelijkeLocatie = false;
					boolean nieuweAfspraakInTijdelijkeLocatie = false;
					if (!DateUtil.compareBefore(laatsteAfspraak.getVanaf(), tijdelijkeLocatie.getStartDatum())
						&& !DateUtil.compareAfter(laatsteAfspraak.getVanaf(), tijdelijkeLocatie.getEindDatum()))
					{
						vorigeAfspraakInTijdelijkeLocatie = true;
					}

					if (!nieuweAfspraakDatum.isBefore(DateUtil.toLocalDate(tijdelijkeLocatie.getStartDatum()))
						&& !nieuweAfspraakDatum.isAfter(DateUtil.toLocalDate(tijdelijkeLocatie.getEindDatum())))
					{
						nieuweAfspraakInTijdelijkeLocatie = true;
					}

					if (vorigeAfspraakInTijdelijkeLocatie != nieuweAfspraakInTijdelijkeLocatie)
					{
						return "andere.locatie.actief.standplaats";
					}
				}
			}
		}
		return "";
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public boolean magBevestigingsbriefAanmaken(MammaAfspraak afspraak)
	{
		List<MammaBrief> brieven = afspraak.getUitnodiging().getScreeningRonde().getBrieven();
		for (MammaBrief brief : brieven)
		{
			BriefType briefType = brief.getBriefType();
			if (!DateUtil.compareBefore(brief.getCreatieDatum(), afspraak.getCreatiedatum()) && BriefType.isMammaUitnodigingBrief(briefType))
			{
				return false;
			}
		}
		return true;
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
	public void verzetAfsprakenNaarStandplaatsPlusBrievenKlaarzettenVoorAfdrukken(Map<Long, List<Long>> afsprakenTeVerplaatsen, Account account)
	{
		afsprakenTeVerplaatsen.forEach((key, value) -> EXECUTOR_SERVICE.submit(new AfsprakenVerplaatsenThread(key, value, account.getId())));
	}

	private class AfsprakenVerplaatsenThread extends OpenHibernateSessionInThread
	{

		private final Long standplaatsPeriodeId;

		private final List<Long> afsprakenIds;

		private final Long ingelogdeOrganisatieMedewerkerId;

		AfsprakenVerplaatsenThread(Long standplaatsPeriodeId, List<Long> afsprakenIds, Long ingelogdeOrganisatieMedewerkerId)
		{
			super(true);
			this.standplaatsPeriodeId = standplaatsPeriodeId;
			this.afsprakenIds = afsprakenIds;
			this.ingelogdeOrganisatieMedewerkerId = ingelogdeOrganisatieMedewerkerId;
		}

		@Override
		protected void runInternal()
		{
			List<MammaBrief> brieven = new ArrayList<>();
			MammaStandplaatsPeriode persistentStandplaatsPeriode = hibernateService.get(MammaStandplaatsPeriode.class, standplaatsPeriodeId);
			OrganisatieMedewerker ingelogdeOrganisatieMedewerker = hibernateService.get(OrganisatieMedewerker.class, ingelogdeOrganisatieMedewerkerId);
			PlanningVerzetClientenDto verzetClientenDto = new PlanningVerzetClientenDto();
			verzetClientenDto.verzetStandplaatsPeriodeId = persistentStandplaatsPeriode.getId();
			Set<LocalDate> afspraakDatums = new HashSet<>();
			for (Long afspraakId : afsprakenIds)
			{
				MammaAfspraak afspraak = hibernateService.get(MammaAfspraak.class, afspraakId);

				if (afspraak.equals(MammaScreeningRondeUtil.getLaatsteAfspraak(afspraak.getUitnodiging().getScreeningRonde())))
				{
					baseAfspraakService.maakAfspraak(afspraak.getUitnodiging().getScreeningRonde(), afspraak.getCapaciteitBlok(), afspraak.getVanaf(),
						persistentStandplaatsPeriode, MammaVerzettenReden.ONVOORZIENE_OMSTANDIGHEDEN, true, false, false, true, true, ingelogdeOrganisatieMedewerker, false);
					brieven.add(baseBriefService.maakBvoBrief(afspraak.getUitnodiging().getScreeningRonde(), BriefType.MAMMA_AFSPRAAK_VERZET));
					verzetClientenDto.clientIdSet.add(afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient().getId());
					afspraakDatums.add(DateUtil.toLocalDate(afspraak.getVanaf()));
				}
			}
			if (!brieven.isEmpty())
			{
				baseConceptPlanningsApplicatie.verzetClienten(verzetClientenDto);
				berichtToSeRestBkService.notificeerScreeningsEenheidVerversenDaglijst(persistentStandplaatsPeriode.getScreeningsEenheid(), afspraakDatums);
				baseStandplaatsService.zetBrievenKlaarVoorStandplaatsVoorAfdrukken(brieven, persistentStandplaatsPeriode.getStandplaatsRonde().getStandplaats());
			}
		}

	}
}
