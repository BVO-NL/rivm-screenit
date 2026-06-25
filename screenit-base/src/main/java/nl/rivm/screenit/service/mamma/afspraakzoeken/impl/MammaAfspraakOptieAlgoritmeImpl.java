package nl.rivm.screenit.service.mamma.afspraakzoeken.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.MammaBaseAfspraakOptieDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptieAlgoritme;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaOnvoldoendeVrijeCapaciteitException;
import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaStandplaatsPeriodeMetZoekbereik;
import nl.rivm.screenit.service.mamma.impl.MammaCapaciteit;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningsEenheidUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

@Component("mammaAfspraakOptieAlgoritme")
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
@Slf4j
@RequiredArgsConstructor
public class MammaAfspraakOptieAlgoritmeImpl implements MammaAfspraakOptieAlgoritme
{
	private static final BigDecimal TEN = new BigDecimal("10");

	private final MammaBaseCapaciteitsBlokService capaciteitsBlokService;

	private final MammaBaseDossierService dossierService;

	private final SimplePreferenceService simplePreferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private List<MammaStandplaatsPeriodeMetZoekbereik> standplaatsPeriodesMetZoekbereik;

	private MammaAfspraakOptieZoekContext zoekContext;

	private boolean extraOpties = false;

	private boolean enkeleOptie = false;

	private MammaCapaciteit capaciteit;

	private List<MammaRationaalDag> rationaalDagen;

	@Override
	public List<MammaAfspraakOptie> getAfspraakOpties(MammaDossier dossier, MammaStandplaatsPeriodeMetZoekbereik standplaatsPeriodeMetZoekbereik,
		boolean extraOpties, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		return getAfspraakOpties(dossier, standplaatsPeriodeMetZoekbereik, extraOpties, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen, false);
	}

	@Override
	public List<MammaAfspraakOptie> getAfspraakOptieBulkVerzetten(MammaDossier dossier, MammaStandplaatsPeriodeMetZoekbereik standplaatsPeriodeMetZoekbereik,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		return getAfspraakOpties(dossier, standplaatsPeriodeMetZoekbereik, false, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen, true);
	}

	private List<MammaAfspraakOptie> getAfspraakOpties(MammaDossier dossier, MammaStandplaatsPeriodeMetZoekbereik standplaatsPeriodeMetZoekbereik,
		boolean extraOpties, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen, boolean enkeleOptie)
	{
		this.extraOpties = extraOpties;
		this.enkeleOptie = enkeleOptie;
		this.standplaatsPeriodesMetZoekbereik = List.of(standplaatsPeriodeMetZoekbereik);
		initialiseerZoekContext(dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);

		return zoekAfspraakOpties();
	}

	@Override
	public MammaAfspraakOptie getAfspraakOptieUitnodiging(MammaDossier dossier, List<MammaStandplaatsPeriodeMetZoekbereik> standplaatsPeriodesMetZoekbereik,
		BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
		throws MammaOnvoldoendeVrijeCapaciteitException
	{
		this.standplaatsPeriodesMetZoekbereik = standplaatsPeriodesMetZoekbereik;
		initialiseerZoekContext(dossier, voorlopigeOpkomstkans, capaciteitVolledigBenutTotEnMetAantalWerkdagen);
		enkeleOptie = true;

		var afspraakOpties = zoekAfspraakOpties();
		if (afspraakOpties.isEmpty())
		{
			throw new MammaOnvoldoendeVrijeCapaciteitException();
		}
		return afspraakOpties.getFirst();
	}

	private void initialiseerZoekContext(MammaDossier dossier, BigDecimal voorlopigeOpkomstkans, Integer capaciteitVolledigBenutTotEnMetAantalWerkdagen)
	{
		var screeningsEenheid = standplaatsPeriodesMetZoekbereik.getFirst().standplaatsPeriode().getScreeningsEenheid();
		var screeningOrganisatie = MammaScreeningsEenheidUtil.getScreeningsOrganisatie(screeningsEenheid);
		var factor = dossierService.getFactorType(dossier).getFactor(screeningOrganisatie);
		var vrijgevenMindervalideReserveringenBinnenAantalDagen = simplePreferenceService.getInteger(
			PreferenceKey.MAMMA_VRIJGEVEN_MINDERVALIDE_RESERVERINGEN_BINNEN_AANTAL_DAGEN.name());
		var vrijgevenMindervalideReserveringenTotEnMetDatum = currentDateSupplier.getLocalDate().plusDays(vrijgevenMindervalideReserveringenBinnenAantalDagen);
		zoekContext = new MammaAfspraakOptieZoekContext(dossier, factor, voorlopigeOpkomstkans, screeningsEenheid, screeningOrganisatie,
			capaciteitVolledigBenutTotEnMetAantalWerkdagen, vrijgevenMindervalideReserveringenTotEnMetDatum);
	}

	private List<MammaAfspraakOptie> zoekAfspraakOpties()
	{
		initialiseerRationaalDagen();

		var totaleVrijeCapaciteit = rationaalDagen.stream().map(MammaRationaalDag::getVrijeCapaciteit).reduce(BigDecimal.ZERO, BigDecimal::add);
		var aantalOptiesVrijeCapaciteit = deelEnRondAfNaarBoven(totaleVrijeCapaciteit, zoekContext.getBenodigdeCapaciteit());
		var maxAantalOpties = maximumAantalTeVindenOpties(aantalOptiesVrijeCapaciteit);

		LOG.debug("totaleVrijeCapaciteit: {}, benodigdeCapaciteit: {}, aantalOptiesVrijeCapaciteit: {}, maxAantalOpties: {}, extraOpties: {}",
			totaleVrijeCapaciteit, zoekContext.getBenodigdeCapaciteit(), aantalOptiesVrijeCapaciteit, maxAantalOpties, extraOpties);

		var streefcapaciteitCalculator = new MammaStreefcapaciteitCalculator(rationaalDagen);
		var afspraakOpties = new ArrayList<MammaAfspraakOptie>();
		for (var i = 0; i < aantalOptiesVrijeCapaciteit; i++)
		{
			if (afspraakOpties.size() >= maxAantalOpties)
			{
				break;
			}

			streefcapaciteitCalculator.updateStreefCapaciteit(); 
			var afspraakOptie = MammaCapaciteitZoeken.elementMetRelatiefMeesteVrijeCapaciteit(rationaalDagen).getAfspraakOptie();
			if (afspraakOptie.isGeldigeAfspraak())
			{
				afspraakOpties.add(new MammaBaseAfspraakOptieDto(afspraakOptie.getCapaciteitBlokId(), afspraakOptie.getDatumTijd(), afspraakOptie.getStandplaatsPeriodeId()));
			}
		}
		return afspraakOpties;
	}

	private int maximumAantalTeVindenOpties(int aantalOptiesVrijeCapaciteit)
	{
		if (enkeleOptie)
		{
			return 1;
		}
		return extraOpties ? aantalOptiesVrijeCapaciteit
			: deelEnRondAfNaarBoven(capaciteit.getBeschikbareCapaciteit(), TEN);
	}

	private int deelEnRondAfNaarBoven(BigDecimal teller, BigDecimal noemer)
	{
		return teller.divide(noemer, 5, RoundingMode.HALF_UP).setScale(0, RoundingMode.UP).intValue();
	}

	private void initialiseerRationaalDagen()
	{
		var nietGeblokkeerdeCapaciteitBlokken = getNietGeblokkeerdeCapaciteitBlokkenInZoekPeriode();

		capaciteit = capaciteitsBlokService.getCapaciteit(nietGeblokkeerdeCapaciteitBlokken);

		var huidigeDagVoorPlannenAfspraken = MammaPlanningUtil.bepaalHuidigeDagVoorPlannenAfspraken(currentDateSupplier.getLocalDateTime());
		var aflopendVanaf = DateUtil.plusWerkdagen(huidigeDagVoorPlannenAfspraken, zoekContext.getCapaciteitVolledigBenutTotEnMetAantalWerkdagen());

		var capaciteitBlokkenPerDag = groepeerEnSorteerCapaciteitBlokkenPerDag(nietGeblokkeerdeCapaciteitBlokken);

		rationaalDagen = capaciteitBlokkenPerDag.keySet().stream()
			.map(datum -> new MammaRationaalDag(capaciteitBlokkenPerDag.get(datum), getAflopendDagNummer(datum, aflopendVanaf), zoekContext))
			.filter(MammaRationaalDag::vrijeCapaciteitNietNegatief)
			.toList();
	}

	private List<MammaCapaciteitBlokDto> getNietGeblokkeerdeCapaciteitBlokkenInZoekPeriode()
	{
		var client = zoekContext.getDossier().getClient();
		return standplaatsPeriodesMetZoekbereik.stream()
			.map(spp -> capaciteitsBlokService.getNietGeblokkeerdeScreeningCapaciteitBlokDtos(spp.standplaatsPeriode(), spp.vanafUtilDate(), spp.totEnMetUtilDate(), client))
			.flatMap(Collection::stream)
			.toList();
	}

	private TreeMap<LocalDate, List<MammaCapaciteitBlokDto>> groepeerEnSorteerCapaciteitBlokkenPerDag(List<MammaCapaciteitBlokDto> capaciteitBlokken)
	{
		return capaciteitBlokken.stream()
			.sorted(comparing(MammaCapaciteitBlokDto::getVanaf))
			.collect(groupingBy(
				MammaCapaciteitBlokDto::getDatum,
				TreeMap::new,
				toList()));
	}

	private BigDecimal getAflopendDagNummer(LocalDate datum, LocalDate aflopendVanaf)
	{
		return BigDecimal.valueOf(ChronoUnit.DAYS.between(aflopendVanaf, datum.plusDays(1L)));
	}
}
