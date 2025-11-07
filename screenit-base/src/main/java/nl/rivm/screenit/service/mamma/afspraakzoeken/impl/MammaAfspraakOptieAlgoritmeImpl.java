package nl.rivm.screenit.service.mamma.afspraakzoeken.impl;

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
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaScreeningsEenheidDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptieAlgoritme;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaCapaciteitZoeken;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaOnvoldoendeVrijeCapaciteitException;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaRationaal;
import nl.rivm.screenit.service.mamma.impl.MammaCapaciteit;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.TimeRange;

import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Component("mammaAfspraakOptieAlgoritme") 
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
@Slf4j
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaAfspraakOptieAlgoritmeImpl implements MammaAfspraakOptieAlgoritme
{
	@Autowired
	private MammaBaseCapaciteitsBlokService capaciteitsBlokService;

	@Autowired
	private MammaBaseDossierService dossierService;

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	private static final BigDecimal TEN = new BigDecimal("10");

	private static final BigDecimal MINIMUM_OPKOMSTKANS = new BigDecimal("0.1");

	private boolean meerdereOpties = false;

	private List<MammaStandplaatsPeriode> standplaatsPeriodeList;

	private final Map<MammaStandplaatsPeriode, LocalDate> standplaatsPeriodeVanafMap = new HashMap<>();

	private final Map<MammaStandplaatsPeriode, LocalDate> standplaatsPeriodeTotEnMetMap = new HashMap<>();

	private BigDecimal factor;

	private BigDecimal benodigdeCapaciteit;

	private boolean extraOpties = false;

	private LocalDate vandaagOfMorgen;

	private LocalDate aflopendVanaf;

	private MammaCapaciteit capaciteit;

	private Map<LocalDate, List<MammaCapaciteitBlokDto>> dateCapaciteitBlokMap = new TreeMap<>();

	private List<DeterminatieDag> determinatieDagList = new ArrayList<>();

	private MammaScreeningsEenheidDto screeningsEenheidDto; 

	private int minimaleDagCapaciteitMinderValideAfspraken;

	private boolean isMindervalide;

	private boolean isDubbeleTijd;

	private void init(List<MammaStandplaatsPeriode> standplaatsPeriodeList, MammaDossier dossier, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		this.standplaatsPeriodeList = standplaatsPeriodeList;
		isMindervalide = dossier.getDoelgroep().equals(MammaDoelgroep.MINDER_VALIDE);
		isDubbeleTijd = dossier.getDoelgroep().equals(MammaDoelgroep.DUBBELE_TIJD) || dossier.getTehuis() != null;

		var standplaatsPeriode = standplaatsPeriodeList.get(0);

		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		screeningsEenheidDto = MammaScreeningsEenheidDto.vanEntiteit(screeningsEenheid);

		var screeningOrganisatie = (ScreeningOrganisatie) Hibernate.unproxy(screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio());
		factor = dossierService.getFactorType(dossier).getFactor(screeningOrganisatie);
		minimaleDagCapaciteitMinderValideAfspraken = screeningOrganisatie.getMinimaleDagCapaciteitMinderValideAfspraken();

		benodigdeCapaciteit = factor.multiply(voorlopigeOpkomstkans.compareTo(MINIMUM_OPKOMSTKANS) >= 0 ? voorlopigeOpkomstkans : MINIMUM_OPKOMSTKANS);

		vandaagOfMorgen = afspraakService.getHuidigeDagVoorPlannenAfspraken();

		aflopendVanaf = DateUtil.plusWerkdagen(vandaagOfMorgen, capaciteitVolledigBenutTotEnMetAantalWerkdagen);
	}

	@Override
	public List<MammaAfspraakOptie> getAfspraakOpties(MammaDossier dossier, MammaStandplaatsPeriode standplaatsPeriode, LocalDate vanaf, LocalDate totEnMet,
		boolean extraOpties, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, boolean corrigeerNegatieveVrijeCapaciteit)
	{
		init(Collections.singletonList(standplaatsPeriode), dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);
		this.extraOpties = extraOpties || isMindervalide;

		standplaatsPeriodeVanafMap.put(standplaatsPeriode, vanaf);
		standplaatsPeriodeTotEnMetMap.put(standplaatsPeriode, totEnMet);

		meerdereOpties = true;
		return getAfspraakOpties(corrigeerNegatieveVrijeCapaciteit, dossier.getClient());
	}

	@Override
	public MammaAfspraakOptie getAfspraakOptieUitnodiging(MammaDossier dossier, MammaStandplaatsRonde standplaatsRonde, BigDecimal voorlopigeOpkomstkans,
		Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, Integer afspraakBijUitnodigenVanafAantalWerkdagen)
		throws MammaOnvoldoendeVrijeCapaciteitException
	{
		var standplaatsPeriodes = standplaatsRonde.getStandplaatsPerioden().stream()
			.filter(standplaatsPeriode -> standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet() != null).collect(Collectors.toList());
		init(standplaatsPeriodes, dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

		var uitnodigenVanafDatum = DateUtil.plusWerkdagen(vandaagOfMorgen, afspraakBijUitnodigenVanafAantalWerkdagen);
		for (var standplaatsPeriode : standplaatsPeriodes)
		{
			standplaatsPeriodeVanafMap.put(standplaatsPeriode, Collections.max(Arrays.asList(uitnodigenVanafDatum, DateUtil.toLocalDate(standplaatsPeriode.getVanaf()))));
			standplaatsPeriodeTotEnMetMap.put(standplaatsPeriode,
				DateUtil.toLocalDate(Collections.min(Arrays.asList(standplaatsPeriode.getScreeningsEenheid().getUitnodigenTotEnMet(), standplaatsPeriode.getTotEnMet()))));
		}

		var afspraakOpties = getAfspraakOpties(false, dossier.getClient());
		if (afspraakOpties.isEmpty())
		{
			throw new MammaOnvoldoendeVrijeCapaciteitException();
		}
		return afspraakOpties.get(0);
	}

	private void bepaalDeterminatiePeriode(Client client)
	{
		var nietGeblokkeerdeCapaciteitsBlokken = new ArrayList<MammaCapaciteitBlokDto>();

		for (var standplaatsPeriode : standplaatsPeriodeList)
		{
			var vanafDate = DateUtil.toUtilDate(standplaatsPeriodeVanafMap.get(standplaatsPeriode));
			var totEnMetDate = DateUtil.toUtilDate(standplaatsPeriodeTotEnMetMap.get(standplaatsPeriode).atTime(Constants.BK_EINDTIJD_DAG));

			var standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken = capaciteitsBlokService.getNietGeblokkeerdeScreeningCapaciteitBlokDtos(standplaatsPeriode,
				vanafDate, totEnMetDate, client);

			nietGeblokkeerdeCapaciteitsBlokken.addAll(standplaatsPeriodeNietGeblokkeerdeCapaciteitsBlokken);
		}

		capaciteit = capaciteitsBlokService.getCapaciteit(nietGeblokkeerdeCapaciteitsBlokken);

		var capaciteitsBlokMap = new TreeMap<LocalDate, List<MammaCapaciteitBlokDto>>();
		nietGeblokkeerdeCapaciteitsBlokken.forEach(blok ->
		{
			var vanafDatum = blok.vanaf.toLocalDate();
			capaciteitsBlokMap.computeIfAbsent(vanafDatum, v -> new ArrayList<>()).add(blok);
		});

		capaciteitsBlokMap.forEach((key, value) -> value.sort(Comparator.comparing(b -> b.vanaf)));

		dateCapaciteitBlokMap = capaciteitsBlokMap;
		determinatieDagList = capaciteitsBlokMap.keySet().stream()
			.map(DeterminatieDag::new)
			.filter(determinatieDag -> determinatieDag.beschikbareCapaciteit.subtract(determinatieDag.benutteCapaciteit).compareTo(BigDecimal.ZERO) >= 0
				&& !determinatieDag.getDeterminatieBlokList().isEmpty())
			.collect(Collectors.toList());
	}

	private List<MammaAfspraakOptie> getAfspraakOpties(boolean corrigeerNegatieveVrijeCapaciteit, Client client)
	{
		bepaalDeterminatiePeriode(client);

		var afspraakOpties = new ArrayList<MammaAfspraakOptie>();
		if (isMindervalide && standplaatsPeriodeList.get(0).getStandplaatsRonde().getMinderValideUitwijkStandplaats() != null)
		{
			return afspraakOpties;
		}

		var totaleVrijeCapaciteit = capaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit);

		if (meerdereOpties && determinatieDagList.isEmpty())
		{
			return afspraakOpties;
		}

		var aantalOptiesVrijeCapaciteit = totaleVrijeCapaciteit.divide(benodigdeCapaciteit, 5, RoundingMode.HALF_UP).setScale(0, RoundingMode.UP).intValue();
		var maxAantalOptiesTonen = capaciteit.getBeschikbareCapaciteit().divide(TEN, 5, RoundingMode.HALF_UP).setScale(0, RoundingMode.UP).intValue();
		var maxZoekPogingen = aantalOptiesVrijeCapaciteit + 1; 

		LOG.debug("totaleVrijeCapaciteit: {}, benodigdeCapaciteit: {}, aantalOptiesVrijeCapaciteit: {}, maxAantalOptiesTonen zonder extraOpties: {}, extraOpties: {}",
			totaleVrijeCapaciteit, benodigdeCapaciteit, aantalOptiesVrijeCapaciteit, maxAantalOptiesTonen, extraOpties);

		for (var i = 0; i < maxZoekPogingen; i++)
		{
			if (aantalOptiesVrijeCapaciteit == 0
				|| !extraOpties && maxAantalOptiesTonen == 0
				|| !meerdereOpties && !afspraakOpties.isEmpty())
			{
				break;
			}

			bepaalDoelCapaciteit(corrigeerNegatieveVrijeCapaciteit);
			var afspraakOptie = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(determinatieDagList).getAfspraakOptie(benodigdeCapaciteit);
			aantalOptiesVrijeCapaciteit--;
			if (afspraakOptie.isGeldigeAfspraak())
			{
				maxAantalOptiesTonen--;
				afspraakOpties.add(afspraakOptie);
			}
		}

		return afspraakOpties;
	}

	private void bepaalDoelCapaciteit(boolean corrigeerNegatieveVrijeCapaciteit)
	{
		BigDecimal aflopendPerDag;
		if (capaciteit.getVrijeCapaciteit(corrigeerNegatieveVrijeCapaciteit).compareTo(BigDecimal.ZERO) <= 0)
		{
			aflopendPerDag = BigDecimal.ZERO;
		}
		else
		{
			var totaleCapaciteitInAflopendePeriode = BigDecimal.ZERO;
			var beschikbareCapaciteitInAflopendePeriode = BigDecimal.ZERO;
			var vrijeCapaciteitInAflopendePeriode = BigDecimal.ZERO;

			for (var determinatieDag : determinatieDagList)
			{
				if (!determinatieDag.datum.isBefore(aflopendVanaf))
				{
					totaleCapaciteitInAflopendePeriode = totaleCapaciteitInAflopendePeriode.add(determinatieDag.aflopendDag.multiply(determinatieDag.beschikbareCapaciteit));
					beschikbareCapaciteitInAflopendePeriode = beschikbareCapaciteitInAflopendePeriode.add(determinatieDag.beschikbareCapaciteit);
					vrijeCapaciteitInAflopendePeriode = vrijeCapaciteitInAflopendePeriode.add(determinatieDag.beschikbareCapaciteit.subtract(determinatieDag.benutteCapaciteit));
				}
			}

			if (beschikbareCapaciteitInAflopendePeriode.compareTo(BigDecimal.ZERO) == 0)
			{
				aflopendPerDag = BigDecimal.ONE;
			}
			else
			{
				var aflopendGemiddeldeDag = totaleCapaciteitInAflopendePeriode.divide(beschikbareCapaciteitInAflopendePeriode, 10, RoundingMode.HALF_UP);

				if (aflopendGemiddeldeDag.compareTo(BigDecimal.ZERO) != 0)
				{
					var aflopendGemiddeldeDoel = vrijeCapaciteitInAflopendePeriode.divide(beschikbareCapaciteitInAflopendePeriode, 10, RoundingMode.HALF_UP);

					aflopendPerDag = aflopendGemiddeldeDoel.divide(aflopendGemiddeldeDag, 10, RoundingMode.HALF_UP);
				}
				else
				{
					aflopendPerDag = BigDecimal.ONE;
				}
			}
		}
		determinatieDagList.forEach(determinatieDag -> determinatieDag.bepaalDoelCapaciteit(aflopendPerDag));
	}

	private class DeterminatieDag extends MammaRationaal
	{
		private final LocalDate datum;

		private BigDecimal beschikbareCapaciteit = BigDecimal.ZERO;

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private BigDecimal doelCapaciteit = BigDecimal.ZERO;

		@Getter
		private final List<DeterminatieBlok> determinatieBlokList = new ArrayList<>();

		private final BigDecimal aflopendDag;

		private final boolean genoegDagCapaciteitVoorMinderValide;

		private DeterminatieDag(LocalDate datum)
		{
			this.datum = datum;
			aflopendDag = new BigDecimal(ChronoUnit.DAYS.between(aflopendVanaf, datum.plusDays(1L)));

			var totaleBeschikbareDagCapaciteit = new AtomicReference<>(BigDecimal.ZERO);

			dateCapaciteitBlokMap.get(datum)
				.forEach(capaciteitBlok ->
				{
					var determinatieBlok = new DeterminatieBlok(capaciteitBlok, datum);
					totaleBeschikbareDagCapaciteit.set(totaleBeschikbareDagCapaciteit.get().add(capaciteitBlok.beschikbareCapaciteit));
					determinatieBlokList.add(determinatieBlok);

					beschikbareCapaciteit = beschikbareCapaciteit.add(determinatieBlok.beschikbareCapaciteit);
					benutteCapaciteit = benutteCapaciteit.add(determinatieBlok.benutteCapaciteit);
				});

			genoegDagCapaciteitVoorMinderValide = totaleBeschikbareDagCapaciteit.get().compareTo(BigDecimal.valueOf(minimaleDagCapaciteitMinderValideAfspraken)) >= 0;
		}

		private MammaAfspraakOptieImpl getAfspraakOptie(BigDecimal benodigdeCapaciteit)
		{
			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);

			var determinatieBlok = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(determinatieBlokList);
			LOG.debug("getAfspraakOptie op blok vanaf: {}, beschikbaar: {}, benut: {}, ratio: {}, blokId: {} ", determinatieBlok.capaciteitBlokDto.vanaf,
				determinatieBlok.beschikbareCapaciteit, determinatieBlok.benutteCapaciteit,
				determinatieBlok.getRatioTekst(), determinatieBlok.capaciteitBlokDto.id);
			var afspraakOptie = determinatieBlok.getAfspraakOptie(benodigdeCapaciteit);
			if (afspraakOptie.isMinderValide() && !genoegDagCapaciteitVoorMinderValide)
			{
				afspraakOptie.setGeldigeAfspraak(false);
			}
			return afspraakOptie;
		}

		private void bepaalDoelCapaciteit(BigDecimal aflopendPerDag)
		{
			BigDecimal doel;
			if (datum.isBefore(aflopendVanaf))
			{
				doel = BigDecimal.ONE;
			}
			else
			{
				doel = BigDecimal.ONE.subtract(aflopendPerDag.multiply(aflopendDag));

				if (doel.compareTo(BigDecimal.ZERO) <= 0)
				{
					doel = new BigDecimal("0.0001");
				}
			}

			doelCapaciteit = beschikbareCapaciteit.multiply(doel);
		}

		@Override
		public BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		public BigDecimal getDeler()
		{
			return doelCapaciteit;
		}
	}

	private class DeterminatieBlok extends MammaRationaal
	{
		private final MammaCapaciteitBlokDto capaciteitBlokDto;

		private final LocalDate datum;

		private final BigDecimal beschikbareCapaciteit; 

		private BigDecimal benutteCapaciteit = BigDecimal.ZERO;

		private final List<MammaAfspraakOptieImpl> afspraakOptiesInBlok = new ArrayList<>();

		private final boolean heeftVrijeCapaciteit;

		private DeterminatieBlok(MammaCapaciteitBlokDto capaciteitBlokDto, LocalDate datum)
		{
			this.capaciteitBlokDto = capaciteitBlokDto;
			this.datum = datum;

			LOG.debug("{} afspraken in capaciteitBlok {} vanaf {}, beschikbaar: {}, vrij: {}", capaciteitBlokDto.afspraakDtos.size(), capaciteitBlokDto.id,
				capaciteitBlokDto.vanaf, capaciteitBlokDto.beschikbareCapaciteit, capaciteitBlokDto.vrijeCapaciteit);

			beschikbareCapaciteit = capaciteitBlokDto.beschikbareCapaciteit;

			capaciteitBlokDto.afspraakDtos.forEach(afspraak ->
			{
				var afspraakOptie = new MammaAfspraakOptieImpl(capaciteitBlokDto,
					TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot),
					TimeRange.of(afspraak.getVanaf().toLocalTime(), afspraak.getTot()), afspraak.getBenodigdeCapaciteit(), screeningsEenheidDto,
					afspraak.isMinderValide(), afspraak.isDubbeleTijd());
				afspraakOptie.setGeldigeAfspraak(true);
				addAfspraakOptie(afspraakOptie);
				benutteCapaciteit = benutteCapaciteit.add(afspraak.getBenodigdeCapaciteit());
			});

			heeftVrijeCapaciteit = beschikbareCapaciteit.subtract(benutteCapaciteit).compareTo(BigDecimal.ZERO) > 0;
		}

		private MammaAfspraakOptieImpl getAfspraakOptie(BigDecimal benodigdeCapaciteit)
		{
			benutteCapaciteit = benutteCapaciteit.add(benodigdeCapaciteit);

			if (!datum.isBefore(aflopendVanaf))
			{
				var blokTypeCapaciteit = capaciteit;
				blokTypeCapaciteit.setBenutteCapaciteit(blokTypeCapaciteit.getBenutteCapaciteit().add(benodigdeCapaciteit));
				blokTypeCapaciteit.setVrijeCapaciteit(blokTypeCapaciteit.getVrijeCapaciteit().subtract(benodigdeCapaciteit));
				if (blokTypeCapaciteit.getVrijeCapaciteit().compareTo(BigDecimal.ZERO) < 0)
				{
					blokTypeCapaciteit.setNegatieveVrijeCapaciteit(blokTypeCapaciteit.getVrijeCapaciteit());
				}
			}

			MammaAfspraakOptieImpl afspraakOptie;
			var capaciteitBlokVanaf = capaciteitBlokDto.vanaf;
			if (afspraakOptiesInBlok.isEmpty())
			{

				afspraakOptie = new MammaAfspraakOptieImpl(capaciteitBlokDto, TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot),
					TimeRange.of(capaciteitBlokVanaf.toLocalTime(), capaciteitBlokDto.tot), benodigdeCapaciteit, screeningsEenheidDto, isMindervalide, isDubbeleTijd);
			}
			else
			{
				var eersteAfspraakOptieVanaf = afspraakOptiesInBlok.get(0).getVanaf();
				if (!eersteAfspraakOptieVanaf.equals(capaciteitBlokVanaf.toLocalTime()))
				{

					afspraakOptie = new MammaAfspraakOptieImpl(capaciteitBlokDto,
						TimeRange.of(capaciteitBlokDto.vanaf.toLocalTime(), capaciteitBlokDto.tot),
						TimeRange.of(capaciteitBlokVanaf.toLocalTime(), eersteAfspraakOptieVanaf), benodigdeCapaciteit, screeningsEenheidDto, isMindervalide, isDubbeleTijd);
				}
				else
				{
					afspraakOptie = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(afspraakOptiesInBlok)
						.splitsNieuweAfspraakOptie(benodigdeCapaciteit, factor, isMindervalide, isDubbeleTijd);
				}
			}

			if (!heeftVrijeCapaciteit)
			{
				afspraakOptie.setGeldigeAfspraak(false);
			}

			return addAfspraakOptie(afspraakOptie);
		}

		private MammaAfspraakOptieImpl addAfspraakOptie(MammaAfspraakOptieImpl afspraakOptie)
		{
			afspraakOptiesInBlok.add(afspraakOptie);
			afspraakOptiesInBlok.sort(Comparator.comparing(MammaAfspraakOptie::getVanaf));
			return afspraakOptie;
		}

		@Override
		public BigDecimal getDeeltal()
		{
			return benutteCapaciteit;
		}

		@Override
		public BigDecimal getDeler()
		{
			return beschikbareCapaciteit;
		}

	}
}
