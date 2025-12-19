package nl.rivm.screenit.service.mamma.impl;

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
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaAfspraakReserveringView;
import nl.rivm.screenit.dto.mamma.afspraken.MammaCapaciteitBlokDto;
import nl.rivm.screenit.dto.mamma.afspraken.MammaMindervalideReserveringProjectie;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok_;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.repository.mamma.MammaAfspraakReserveringRepository;
import nl.rivm.screenit.repository.mamma.MammaCapaciteitBlokProjectie;
import nl.rivm.screenit.repository.mamma.MammaCapaciteitBlokRepository;
import nl.rivm.screenit.repository.mamma.MammaMindervalideReserveringRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.service.mamma.MammaBaseConceptPlanningsApplicatie;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaPlanningUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestClientException;

import com.google.common.collect.Range;

import static nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType.GEEN_SCREENING;
import static nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType.SCREENING;
import static nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification.heeftBlokType;
import static nl.rivm.screenit.specification.mamma.MammaCapaciteitBlokSpecification.voorScreeningsEenheidInPeriode;
import static nl.rivm.screenit.util.mamma.MammaScreeningsEenheidUtil.getScreeningsOrganisatie;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
@Slf4j
public class MammaBaseCapaciteitsBlokServiceImpl implements MammaBaseCapaciteitsBlokService
{
	@Autowired
	private MammaCapaciteitBlokRepository capaciteitBlokRepository;

	@Autowired(required = false)
	private MammaBaseConceptPlanningsApplicatie conceptPlanningsApplicatie;

	@Autowired
	private MammaBaseAfspraakService afspraakService;

	@Autowired
	private MammaAfspraakReserveringRepository afspraakReserveringRepository;

	@Autowired
	private MammaMindervalideReserveringRepository mindervalideReserveringRepository;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	public String saveOrUpdate(PlanningCapaciteitBlokDto blok, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		if (GEEN_SCREENING.equals(blok.blokType))
		{
			blok.aantalOnderzoeken = 0;
			blok.minderValideAfspraakMogelijk = false;
			blok.getMinderValideReserveringen().clear();
		}

		var isNieuw = blok.conceptId == null;
		try
		{
			conceptPlanningsApplicatie.sendCapaciteitBlok(blok, isNieuw, ingelogdeOrganisatieMedewerker);
		}
		catch (HttpClientErrorException | HttpServerErrorException se)
		{
			var responseBody = se.getResponseBodyAsString();
			LOG.error("Er ging iets mis bij het opslaan van de capaciteitsblok", se);
			return responseBody;
		}
		catch (RestClientException e)
		{
			return e.getMessage();
		}
		return null;
	}

	@Override
	public String delete(PlanningCapaciteitBlokDto blok, OrganisatieMedewerker ingelogdeOrganisatieMedewerker)
	{
		return conceptPlanningsApplicatie.deleteCapaciteitBlok(blok, ingelogdeOrganisatieMedewerker);
	}

	@Override
	public int getAantalAfsprakenOpBlok(PlanningCapaciteitBlokDto blokDto, boolean toDelete)
	{
		return conceptPlanningsApplicatie.getAantalAfsprakenOpBlok(blokDto, toDelete);
	}

	@Override
	public List<MammaCapaciteitBlok> getAlleCapaciteitBlokken(MammaScreeningsEenheid screeningsEenheid, Range<Date> zoekbereik)
	{
		var capaciteitsBlokken = capaciteitBlokRepository.findAll(voorScreeningsEenheidInPeriode(screeningsEenheid, zoekbereik), Sort.by(MammaCapaciteitBlok_.VANAF));
		bepaalCapaciteit(capaciteitsBlokken, screeningsEenheid);
		return capaciteitsBlokken;
	}

	@Override
	public List<MammaCapaciteitBlok> getScreeningCapaciteitBlokken(MammaScreeningsEenheid screeningsEenheid, Range<Date> zoekbereik, boolean bepaalCapaciteit)
	{
		var capaciteitsBlokken = capaciteitBlokRepository.findAll(voorScreeningsEenheidInPeriode(screeningsEenheid, zoekbereik).and(heeftBlokType(SCREENING)),
			Sort.by(MammaCapaciteitBlok_.VANAF));

		if (bepaalCapaciteit)
		{
			bepaalCapaciteit(capaciteitsBlokken, screeningsEenheid);
		}

		return capaciteitsBlokken;
	}

	@Override
	public List<MammaCapaciteitBlok> getGeenScreeningCapaciteitBlokken(MammaScreeningsEenheid screeningsEenheid, Range<Date> zoekbereik)
	{
		return capaciteitBlokRepository.findAll(voorScreeningsEenheidInPeriode(screeningsEenheid, zoekbereik).and(heeftBlokType(GEEN_SCREENING)),
			Sort.by(MammaCapaciteitBlok_.VANAF));
	}

	private void bepaalCapaciteit(List<MammaCapaciteitBlok> capaciteitsBlokken, MammaScreeningsEenheid screeningsEenheid)
	{
		var screeningOrganisatie = getScreeningsOrganisatie(screeningsEenheid);
		for (var capaciteitBlok : capaciteitsBlokken)
		{
			var aantalOnderzoeken = new BigDecimal(capaciteitBlok.getAantalOnderzoeken());
			capaciteitBlok.setBeschikbareCapaciteit(aantalOnderzoeken.multiply(MammaFactorType.GEEN.getFactor(screeningOrganisatie)));

			afspraakService.bepaalBenodigdeCapaciteit(capaciteitBlok.getAfspraken(), screeningsEenheid);
			var benodigdeCapaciteit = afspraakService.getBenodigdeCapaciteit(capaciteitBlok.getAfspraken());
			capaciteitBlok.setVrijeCapaciteit(capaciteitBlok.getBeschikbareCapaciteit().subtract(benodigdeCapaciteit));
		}
	}

	@Override
	public Collection<MammaCapaciteitBlokDto> getNietGeblokkeerdeScreeningCapaciteitBlokDtos(MammaStandplaatsPeriode standplaatsPeriode, Date vanaf, Date totEnMet, Client client)
	{
		if (DateUtil.compareAfter(vanaf, totEnMet))
		{

			return Collections.emptyList();
		}

		var screeningOrganisatie = getScreeningsOrganisatie(standplaatsPeriode.getScreeningsEenheid());
		var capaciteitBlokProjecties = getCapaciteitBlokProjecties(standplaatsPeriode, vanaf, totEnMet, screeningOrganisatie);

		var capaciteitBlokDtoPerId = maakCapaciteitBlokDtoMap(capaciteitBlokProjecties, standplaatsPeriode, screeningOrganisatie);
		haalAfspraakReserveringenOpEnVoegToeAanCapaciteitBlokken(capaciteitBlokDtoPerId, screeningOrganisatie, client);
		haalMindervalideReserveringenOpEnVoegToeAanCapaciteitBlokken(capaciteitBlokDtoPerId);
		capaciteitBlokDtoPerId.values().forEach(MammaPlanningUtil::sorteerCapaciteitBlokOpAfspraakTijdEnZetAfspraakTot);
		return capaciteitBlokDtoPerId.values();
	}

	private List<MammaCapaciteitBlokProjectie> getCapaciteitBlokProjecties(MammaStandplaatsPeriode standplaatsPeriode, Date vanaf, Date totEnMet,
		ScreeningOrganisatie screeningOrganisatie)
	{
		var zoekBereik = Range.closed(vanaf, totEnMet);
		var screeningsEenheid = standplaatsPeriode.getScreeningsEenheid();
		var standplaats = standplaatsPeriodeOverlaptMetZoekBereik(standplaatsPeriode, vanaf, totEnMet) ? standplaatsPeriode.getStandplaatsRonde().getStandplaats() : null;

		return capaciteitBlokRepository.findNietGeblokkeerdeScreeningCapaciteitBlokken(zoekBereik, screeningsEenheid, screeningOrganisatie, standplaats);
	}

	private Map<Long, MammaCapaciteitBlokDto> maakCapaciteitBlokDtoMap(List<MammaCapaciteitBlokProjectie> capaciteitBlokProjecties,
		MammaStandplaatsPeriode standplaatsPeriode, ScreeningOrganisatie screeningOrganisatie)
	{
		Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap = new HashMap<>();

		for (var blokProjectie : capaciteitBlokProjecties)
		{
			var capaciteitBlokDto = capaciteitBlokDtoMap.computeIfAbsent(
				blokProjectie.getBlokId(),
				id -> createCapaciteitBlokDto(standplaatsPeriode, blokProjectie, screeningOrganisatie)
			);

			voegAfspraakToeAanBlok(blokProjectie, capaciteitBlokDto, screeningOrganisatie);
		}

		return capaciteitBlokDtoMap;
	}

	private static MammaCapaciteitBlokDto createCapaciteitBlokDto(MammaStandplaatsPeriode standplaatsPeriode, MammaCapaciteitBlokProjectie projectie,
		ScreeningOrganisatie screeningOrganisatie)
	{
		var capaciteitBlokDto = new MammaCapaciteitBlokDto();
		capaciteitBlokDto.setId(projectie.getBlokId());
		capaciteitBlokDto.setVanaf(DateUtil.toLocalDateTime(projectie.getBlokVanaf()));
		capaciteitBlokDto.setTot(DateUtil.toLocalTime((projectie.getBlokTot())));
		capaciteitBlokDto.setAantalOnderzoeken(projectie.getAantalOnderzoeken());
		capaciteitBlokDto.setMinderValideAfspraakMogelijk(projectie.isMinderValideAfspraakMogelijk());
		var aantalOnderzoeken = new BigDecimal(capaciteitBlokDto.getAantalOnderzoeken());
		capaciteitBlokDto.setBeschikbareCapaciteit(aantalOnderzoeken.multiply(MammaFactorType.GEEN.getFactor(screeningOrganisatie)));
		capaciteitBlokDto.setStandplaatsPeriodeId(standplaatsPeriode.getId());
		return capaciteitBlokDto;
	}

	private void voegAfspraakToeAanBlok(MammaCapaciteitBlokProjectie projectie, MammaCapaciteitBlokDto capaciteitBlokDto, ScreeningOrganisatie screeningOrganisatie)
	{
		if (projectie.getAfspraakVanaf() != null)
		{
			var afspraakDto = createAfspraakDto(projectie, screeningOrganisatie);
			afspraakDto.setCapaciteitBlokDto(capaciteitBlokDto);
			capaciteitBlokDto.getAfspraakDtos().add(afspraakDto);
		}
	}

	private static MammaAfspraakDto createAfspraakDto(MammaCapaciteitBlokProjectie projectie, ScreeningOrganisatie screeningOrganisatie)
	{
		var doelgroep = projectie.getDoelgroep();
		var isTehuisClient = projectie.getTehuisId() != null;
		var eersteOnderzoek = projectie.getEersteOnderzoek();
		var opkomstkans = projectie.getOpkomstkans();
		var factor = MammaFactorType.getFactorType(isTehuisClient, doelgroep, eersteOnderzoek).getFactor(screeningOrganisatie);
		var afspraakDto = new MammaAfspraakDto();
		afspraakDto.setVanaf(DateUtil.toLocalDateTime(projectie.getAfspraakVanaf()));
		afspraakDto.setBenodigdeCapaciteit(factor.multiply(opkomstkans));
		afspraakDto.setMindervalide(doelgroep == MammaDoelgroep.MINDER_VALIDE);
		afspraakDto.setDubbeleTijd(doelgroep == MammaDoelgroep.DUBBELE_TIJD || isTehuisClient);
		return afspraakDto;
	}

	private boolean standplaatsPeriodeOverlaptMetZoekBereik(MammaStandplaatsPeriode standplaatsPeriode, Date zoekVanaf, Date zoekTotEnMet)
	{
		return (standplaatsPeriode.getVanaf().before(zoekTotEnMet) || standplaatsPeriode.getVanaf().equals(zoekTotEnMet))
			&& (standplaatsPeriode.getTotEnMet().after(zoekVanaf) || standplaatsPeriode.getTotEnMet().equals(zoekVanaf));
	}

	private void haalAfspraakReserveringenOpEnVoegToeAanCapaciteitBlokken(Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap, ScreeningOrganisatie screeningOrganisatie,
		Client client)
	{
		var maximaleReserveringsTijd = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_RESERVERING_GELDIG_VOOR.name(), 0);
		var vroegstOpTeHalenReservering = currentDateSupplier.getLocalDateTime().minusMinutes(maximaleReserveringsTijd);

		var capaciteitblokIds = capaciteitBlokDtoMap.keySet();
		if (!capaciteitblokIds.isEmpty())
		{
			var clientId = client != null ? client.getId() : null;
			var reserveringDtos = afspraakReserveringRepository.haalReserveringenOpVoorCapaciteitsblokken(vroegstOpTeHalenReservering, capaciteitblokIds, clientId);
			reserveringDtos.forEach(reservering -> converteerReserveringNaarAfspraakInCapaciteitBlok(reservering, capaciteitBlokDtoMap, screeningOrganisatie));
		}
	}

	private void haalMindervalideReserveringenOpEnVoegToeAanCapaciteitBlokken(Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap)
	{
		var capaciteitblokIds = capaciteitBlokDtoMap.keySet();
		if (!capaciteitblokIds.isEmpty())
		{
			var mindervalideReserveringen = mindervalideReserveringRepository.leesMindervalideReserveringen(capaciteitblokIds);
			mindervalideReserveringen.forEach(reservering -> voegReserveringToeAanCapaciteitBlok(reservering, capaciteitBlokDtoMap));
		}
	}

	private static void voegReserveringToeAanCapaciteitBlok(MammaMindervalideReserveringProjectie reservering, Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap)
	{
		var capaciteitBlokDto = capaciteitBlokDtoMap.get(reservering.capaciteitBlokId());
		capaciteitBlokDto.getMindervalideReserveringen().add(reservering.vanaf().toLocalTime());
	}

	private void converteerReserveringNaarAfspraakInCapaciteitBlok(MammaAfspraakReserveringView reservering, Map<Long, MammaCapaciteitBlokDto> capaciteitBlokDtoMap,
		ScreeningOrganisatie screeningOrganisatie)
	{
		var doelgroep = reservering.getDoelgroep();
		var isTehuisClient = reservering.getTehuisId() != null;
		var factor = MammaFactorType.getFactorType(isTehuisClient, doelgroep, reservering.getEersteOnderzoek()).getFactor(screeningOrganisatie);

		var capaciteitBlokDto = capaciteitBlokDtoMap.get(reservering.getCapaciteitBlokId());
		var afspraakDto = new MammaAfspraakDto();
		afspraakDto.setCapaciteitBlokDto(capaciteitBlokDto);
		afspraakDto.setVanaf(DateUtil.toLocalDateTime(reservering.getVanaf()));
		afspraakDto.setBenodigdeCapaciteit(factor.multiply(reservering.getOpkomstkans()));
		afspraakDto.setMindervalide(doelgroep == MammaDoelgroep.MINDER_VALIDE);
		afspraakDto.setDubbeleTijd(doelgroep == MammaDoelgroep.DUBBELE_TIJD || isTehuisClient);
		capaciteitBlokDto.getAfspraakDtos().add(afspraakDto);
	}

	@Override
	public MammaCapaciteit getCapaciteit(Collection<MammaCapaciteitBlokDto> screeningCapaciteitBlokDtos)
	{
		var capaciteit = new MammaCapaciteit();
		Map<String, BigDecimal> vrijeCapaciteitPerStandplaatsPeriodeDag = new HashMap<>();
		for (var capaciteitBlokDto : screeningCapaciteitBlokDtos)
		{
			capaciteit.setBeschikbareCapaciteit(capaciteit.getBeschikbareCapaciteit().add(capaciteitBlokDto.getBeschikbareCapaciteit()));

			var vrijeCapaciteitVanBlok = MammaPlanningUtil.getVrijeCapaciteitVanBlok(capaciteitBlokDto);
			capaciteit.setVrijeCapaciteit(capaciteit.getVrijeCapaciteit().add(vrijeCapaciteitVanBlok));

			var standplaatsPeriodeDag = capaciteitBlokDto.getDatum().format(DateUtil.LOCAL_DATE_FORMAT) + "-" + capaciteitBlokDto.getStandplaatsPeriodeId();

			var vrijeCapaciteitVoorStandplaatsPeriodeDag = vrijeCapaciteitPerStandplaatsPeriodeDag.getOrDefault(standplaatsPeriodeDag, BigDecimal.ZERO)
				.add(vrijeCapaciteitVanBlok);

			vrijeCapaciteitPerStandplaatsPeriodeDag.put(standplaatsPeriodeDag, vrijeCapaciteitVoorStandplaatsPeriodeDag);
		}

		for (var vrijeDagCapaciteit : vrijeCapaciteitPerStandplaatsPeriodeDag.values())
		{
			if (vrijeDagCapaciteit.compareTo(BigDecimal.ZERO) < 0)
			{
				capaciteit.setNegatieveVrijeCapaciteit(capaciteit.getNegatieveVrijeCapaciteit().add(vrijeDagCapaciteit.multiply(BigDecimal.valueOf(-1))));
			}
		}
		capaciteit.setGebruikteCapaciteit(capaciteit.getBeschikbareCapaciteit().subtract(capaciteit.getVrijeCapaciteit()));
		return capaciteit;
	}

	@Override
	public MammaCapaciteitBlok getCapaciteitsBlokOpTijdstipVoorSe(Client client, MammaScreeningsEenheid screeningsEenheid, Date nu)
	{
		var capaciteitBlokkenNu = getScreeningCapaciteitBlokken(screeningsEenheid, Range.closed(nu, nu), false);
		return capaciteitBlokkenNu.stream().findAny().orElse(null);
	}
}
