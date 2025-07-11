package nl.rivm.screenit.main.service.colon.impl;

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

import java.time.DayOfWeek;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.temporal.Temporal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import nl.rivm.screenit.dao.colon.RoosterDao;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.service.colon.RoosterService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonAfspraakslotListViewWrapper;
import nl.rivm.screenit.model.colon.ColonHerhalingsfrequentie;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot_;
import nl.rivm.screenit.repository.colon.ColonAfspraakslotRepository;
import nl.rivm.screenit.repository.colon.ColonTijdslotRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.colon.ColonTijdslotSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.HibernateObjectSpecification.filterId;
import static nl.rivm.screenit.specification.colon.ColonTijdslotSpecification.valtBinnenDatumRange;

@Service
public class RoosterServiceImpl implements RoosterService
{
	@Autowired
	private RoosterDao roosterDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonAfspraakslotRepository afspraakslotRepository;

	@Autowired
	private ColonTijdslotRepository tijdslotRepository;

	@Override
	@Transactional
	public List<ColonAfspraakslotListViewWrapper> getAlleAfspraakslotsInPeriode(String sortProperty, boolean asc, RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getAlleAfspraakslotsInPeriode(sortProperty, asc, filter, intakeLocatie);
	}

	@Override
	public List<ColonTijdslotDto> searchTijdslots(RoosterListViewFilter filter, long intakelocatieId, ColonTijdslotType typeTijdslot)
	{
		refineFilterDates(filter);
		return tijdslotRepository.searchTijdslots(intakelocatieId, DateUtil.toLocalDateTime(filter.getStartDatum()), DateUtil.toLocalDateTime(filter.getEindDatum()),
				filter.getStartTijd(), filter.getEindTijd(), filter.getKamerId(),
				filter.getSqlDagen(), typeTijdslot.name()).stream()
			.map(tuple -> new ColonTijdslotDto(((Long) tuple.get("tijdslotId")), DateUtil.toLocalDateTime((Temporal) tuple.get("startDatum")),
				DateUtil.toLocalDateTime((Temporal) tuple.get("eindDatum")), (String) tuple.get("kamer"),
				((Long) tuple.get("kamerId")), null))
			.collect(Collectors.toList());
	}

	@Override
	@Transactional
	public List<ColonAfspraakslotListViewWrapper> getAfspraakslots(String sortProperty, boolean asc, long first, long count, RoosterListViewFilter filter,
		ColonIntakelocatie intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getAfspraakslots(sortProperty, asc, first, count, filter, intakeLocatie);
	}

	@Override
	@Transactional
	public long getAfspraakslotsCount(RoosterListViewFilter filter, ColonIntakelocatie intakeLocatie)
	{
		filter = filter.clone();
		refineFilterDates(filter);
		return roosterDao.getAfspraakslotsCount(filter, intakeLocatie);
	}

	private void refineFilterDates(RoosterListViewFilter filter)
	{
		filter.setEindDatum(DateUtil.toUtilDate(DateUtil.toLocalDate(filter.getEindDatum()).plusDays(1)));

		if (filter.getStartDatum().after(filter.getEindDatum()))
		{
			filter.setStartDatum((Date) filter.getEindDatum().clone());
		}
		else
		{
			filter.setStartDatum(DateUtil.toUtilDateMidnight(filter.getStartDatum()));
		}
	}

	@Override
	public List<ColonAfspraakslot> getAfspraakslotsInRangeEnKamer(Range<LocalDateTime> range, ColonAfspraakslot afspraakslot)
	{
		return afspraakslotRepository.findAll(ColonTijdslotSpecification.<ColonAfspraakslot> heeftKamer(afspraakslot.getKamer())
				.and(ColonTijdslotSpecification.<ColonAfspraakslot> valtBinnenDatumTijdRange(range)
					.or(filterId(afspraakslot.getId()))),
			Sort.by(Sort.Direction.ASC, ColonTijdslot_.VANAF));
	}

	@Override
	public List<ColonAfspraakslot> getAfspraakslotsInRange(Range<LocalDate> range)
	{
		return afspraakslotRepository.findAll(valtBinnenDatumRange(range), Sort.by(Sort.Direction.ASC, ColonTijdslot_.VANAF));
	}

	@Override
	public void valideerTijdslot(ColonTijdslot tijdslot) throws ValidatieException
	{
		var vanaf = tijdslot.getVanaf();
		var tot = tijdslot.getTot();
		valideerTijdslotStartTijdVoorEindTijd(vanaf, tot);
		valideerTijdslotStartTijdVeelvoud(vanaf);
		valideerTijdslotStartGewijzigdNaarVerleden(tijdslot);
	}

	@Override
	public Optional<ColonAfspraakslot> getAfspraakslot(Long id)
	{
		return afspraakslotRepository.findById(id);
	}

	@Override
	public ColonIntakelocatie getIntakelocatieVanInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		var organisatie = instellingGebruiker.getOrganisatie();
		return (ColonIntakelocatie) HibernateHelper.deproxy(organisatie);
	}

	@Override
	public Range<LocalDateTime> getCurrentViewRange(ColonTijdslotDto tijdslot)
	{
		return Range.open(tijdslot.getVanaf(), tijdslot.getTot());
	}

	@Override
	public Range<LocalDateTime> getCurrentViewRange(ColonTijdslot tijdslot)
	{
		return Range.open(tijdslot.getVanaf(), tijdslot.getTot());
	}

	@Override
	public <S extends ColonTijdslot> List<S> maakHerhalingTijdslotsAan(S tijdslot, ColonHerhalingDto herhalingDto)
	{
		S tijdslotCone;
		var vorigeTijdslot = tijdslot;
		var herhalingTijdslots = new ArrayList<S>();
		var herhalingEindDatum = DateUtil.eindDag(herhalingDto.getEindDatum());
		while (vorigeTijdslot.getVanaf().isBefore(herhalingEindDatum))
		{
			tijdslotCone = getVolgendeTijdslot(vorigeTijdslot, herhalingDto);
			if (tijdslotCone.getVanaf().isBefore(herhalingEindDatum))
			{
				herhalingTijdslots.add(tijdslotCone);
			}
			vorigeTijdslot = tijdslotCone;
		}
		return herhalingTijdslots;
	}

	private <S extends ColonTijdslot> S getVolgendeTijdslot(S tijdslot, ColonHerhalingDto herhalingDto)
	{
		var tijdslotClone = (S) tijdslot.transientClone();

		var volgendeStartTijd = getStartTijdVolgendeTijdslot(tijdslotClone.getVanaf(), herhalingDto);
		var volgendeEindTijd = getEindTijdVolgendeTijdslot(tijdslotClone, volgendeStartTijd);

		tijdslotClone.setVanaf(volgendeStartTijd);
		tijdslotClone.setTot(volgendeEindTijd);
		return tijdslotClone;
	}

	private LocalDateTime getStartTijdVolgendeTijdslot(LocalDateTime huidigeTijdslotStartTijd, ColonHerhalingDto herhalingDto)
	{
		if (herhalingDto.getFrequentie() == ColonHerhalingsfrequentie.DAGELIJKS)
		{
			return getStartTijdVolgendeDagelijkseTijdslot(huidigeTijdslotStartTijd, herhalingDto.isAlleenWerkdagen());
		}
		var interval = herhalingDto.getFrequentie() == ColonHerhalingsfrequentie.TWEE_WEKELIJKS ? 2 : 1;
		return getStartTijdVolgendeWekelijkseTijdslot(huidigeTijdslotStartTijd, interval, herhalingDto.getDagen());
	}

	private LocalDateTime getEindTijdVolgendeTijdslot(ColonTijdslot afspraakslot, LocalDateTime volgendeStartTijd)
	{
		long tijdSlotDuurInSeconden = Duration.between(afspraakslot.getVanaf(), afspraakslot.getTot()).getSeconds();
		return volgendeStartTijd.plusSeconds(tijdSlotDuurInSeconden);
	}

	private LocalDateTime getStartTijdVolgendeDagelijkseTijdslot(LocalDateTime huidigeTijdslotStartTijd, Boolean businessDaysOnly)
	{
		huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.plusDays(1);

		var weekend = Arrays.asList(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY);

		while (Boolean.TRUE.equals(businessDaysOnly) && weekend.contains(huidigeTijdslotStartTijd.getDayOfWeek()))
		{
			huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.plusDays(1);
		}
		return huidigeTijdslotStartTijd;
	}

	private LocalDateTime getStartTijdVolgendeWekelijkseTijdslot(LocalDateTime huidigeTijdslotStartTijd, int perAantalWeken, List<Integer> weekdagen)
	{
		for (var weekDag : weekdagen)
		{
			if (weekDag > huidigeTijdslotStartTijd.getDayOfWeek().getValue())
			{
				huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.with(DayOfWeek.of(weekDag));
				return huidigeTijdslotStartTijd;
			}
		}
		if (!weekdagen.isEmpty())
		{
			huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.with(DayOfWeek.of(weekdagen.get(0)));
		}
		huidigeTijdslotStartTijd = huidigeTijdslotStartTijd.plusWeeks(perAantalWeken);
		return huidigeTijdslotStartTijd;
	}

	private static void valideerTijdslotStartTijdVoorEindTijd(LocalDateTime vanaf, LocalDateTime tot) throws ValidatieException
	{
		if (!tot.isAfter(vanaf))
		{
			throw new ValidatieException("error.eind.voor.start");
		}
	}

	private static void valideerTijdslotStartTijdVeelvoud(LocalDateTime startTijd) throws ValidatieException
	{
		if (startTijd.getMinute() % 5 != 0)
		{
			throw new ValidatieException("error.minuten.veelvoud.vijf");
		}
	}

	private void valideerTijdslotStartGewijzigdNaarVerleden(ColonTijdslot tijdslot) throws ValidatieException
	{
		var vanaf = tijdslot.getVanaf();
		var tijdslotId = tijdslot.getId();
		var nu = currentDateSupplier.getLocalDateTime();
		if (tijdslotId != null && vanaf.isBefore(nu))
		{
			throw new ValidatieException("error.start.in.verleden");
		}
		else if (tijdslotId == null && vanaf.isBefore(nu))
		{
			throw new ValidatieException("error.nieuwe.start.in.verleden");
		}
	}
}
