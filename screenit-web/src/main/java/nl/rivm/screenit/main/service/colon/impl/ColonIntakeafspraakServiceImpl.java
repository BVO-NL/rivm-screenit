package nl.rivm.screenit.main.service.colon.impl;

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

import java.math.BigDecimal;
import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.service.colon.ColonIntakeafspraakService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.WerklijstIntakeFilter;
import nl.rivm.screenit.model.colon.dto.VrijSlotZonderKamer;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.repository.colon.ColonIntakeAfspraakRepository;
import nl.rivm.screenit.repository.colon.ColonIntakelocatieRepository;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.rivm.screenit.specification.colon.ColonIntakeAfspraakSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
public class ColonIntakeafspraakServiceImpl implements ColonIntakeafspraakService
{
	private final ColonIntakeAfspraakRepository intakeAfspraakRepository;

	private final ColonIntakelocatieRepository intakelokatieRepository;

	private final PlanningService planningService;

	private final OrganisatieParameterService organisatieParameterService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final ColonBaseAfspraakService baseAfspraakService;

	private final ClientContactService clientContactService;

	private Specification<ColonIntakeAfspraak> getSpecification(ColonIntakelocatie intakelocatie, WerklijstIntakeFilter zoekObject)
	{
		return ColonIntakeAfspraakSpecification.heeftGeenVerslagen()
			.and(ColonIntakeAfspraakSpecification.heeftClientNietOverledenOfVerhuisdVoorColoscopie())
			.and(ColonIntakeAfspraakSpecification.heeftConclusieType(ColonConclusieType.COLOSCOPIE))
			.and(ColonIntakeAfspraakSpecification.heeftIntakelocatie(intakelocatie))
			.and(ColonIntakeAfspraakSpecification.heeftConclusieInVerleden(currentDateSupplier.getDateMidnight()))

			.and(ColonIntakeAfspraakSpecification.metFilter(zoekObject));
	}

	@Override
	public List<ColonIntakeAfspraak> getAfsprakenZonderVerslag(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie, long first, long count, Sort sort)
	{
		return intakeAfspraakRepository.findWith(getSpecification(intakelocatie, zoekObject), q -> q.sortBy(sort)).all(first, count);
	}

	@Override
	public long getAantalAfsprakenZonderVerslag(WerklijstIntakeFilter zoekObject, ColonIntakelocatie intakelocatie)
	{
		return intakeAfspraakRepository.count(getSpecification(intakelocatie, zoekObject));
	}

	@Override
	public long countAfsprakenOpDagVanDeWeek(DayOfWeek dagVanDeWeek)
	{
		var result = intakeAfspraakRepository.countColonIntakeAfsprakenOpDag(dagVanDeWeek.getValue());
		return result.getFirst();
	}

	@Override
	public long countAfsprakenInNacht(LocalTime startTijd, LocalTime eindTijd)
	{
		var result = intakeAfspraakRepository.countColonIntakeAfsprakenInNacht(eindTijd, startTijd);
		return result.getFirst();
	}

	@Override
	@Transactional
	public ColonIntakeAfspraak verplaatsAfspraak(Client client, VrijSlotZonderKamer gekozenVrijSlotZonderKamer, LocalDateTime datumTijdBuitenRooster, BriefType briefType,
		boolean briefTegenhouden, boolean verwezenMedischeRedenenDoorInfolijn, String opmerking, OrganisatieMedewerker organisatieMedewerker)
	{
		var nieuweAfspraak = maakNieuweAfspraak(client, gekozenVrijSlotZonderKamer, datumTijdBuitenRooster);
		var buitenRooster = datumTijdBuitenRooster != null;

		baseAfspraakService.verplaatsAfspraak(nieuweAfspraak, organisatieMedewerker, briefType, briefTegenhouden, !buitenRooster, verwezenMedischeRedenenDoorInfolijn);

		clientContactService.maakClientContact(client, currentDateSupplier.getLocalDateTime(), List.of(ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN),
			opmerking, organisatieMedewerker);

		return nieuweAfspraak;
	}

	@Override
	public ColonIntakeAfspraak maakNieuweAfspraak(Client client, VrijSlotZonderKamer gekozenVrijSlotZonderKamer, LocalDateTime datumTijdBuitenRooster)
	{
		if (gekozenVrijSlotZonderKamer.getIntakelocatieId() == null || (gekozenVrijSlotZonderKamer.getStartTijd() == null && datumTijdBuitenRooster == null))
		{
			throw new IllegalStateException("error.colon.intakeafspraak.onvolledige.gegevens");
		}
		var intakelocatie = intakelokatieRepository.findById(gekozenVrijSlotZonderKamer.getIntakelocatieId())
			.orElseThrow(() -> new EntityNietGevondenException("Intakelocatie", gekozenVrijSlotZonderKamer.getIntakelocatieId()));

		ColonIntakekamer beschikbareKamer;
		var buitenRooster = datumTijdBuitenRooster != null;

		if (buitenRooster)
		{
			var kamers = intakelocatie.getKamers();
			beschikbareKamer = kamers.stream().filter(k -> Boolean.TRUE.equals(k.getActief())).findFirst()
				.orElseThrow(() -> new IllegalStateException("error.colon.intakeafspraak.geen.actieve.kamer"));
		}
		else
		{
			beschikbareKamer = planningService.getBeschikbareKamer(DateUtil.toLocalDateTime(gekozenVrijSlotZonderKamer.getStartTijd()),
				gekozenVrijSlotZonderKamer.getIntakelocatieId());
			if (beschikbareKamer == null)
			{
				throw new IllegalStateException("error.colon.intakeafspraak.slot.bezet");
			}
		}

		var nieuweAfspraak = new ColonIntakeAfspraak();
		nieuweAfspraak.setKamer(beschikbareKamer);
		nieuweAfspraak.setBezwaar(false);
		nieuweAfspraak.setGewijzigdOp(currentDateSupplier.getLocalDateTime());
		nieuweAfspraak.setAangemaaktOp(currentDateSupplier.getLocalDateTime());
		nieuweAfspraak.setStatus(ColonAfspraakStatus.GEPLAND);

		if (gekozenVrijSlotZonderKamer.getAfstand() != null)
		{
			nieuweAfspraak.setAfstand(BigDecimal.valueOf(gekozenVrijSlotZonderKamer.getAfstand()));
		}
		else
		{
			nieuweAfspraak.setAfstand(BigDecimal.valueOf(45));
		}

		nieuweAfspraak.setScreeningRonde(client.getColonDossier().getLaatsteScreeningRonde());
		nieuweAfspraak.setClient(client);
		client.getAfspraken().add(nieuweAfspraak);

		if (buitenRooster)
		{
			nieuweAfspraak.setVanaf(DateUtil.toLocalDateTime(datumTijdBuitenRooster));
			var duurAfspraakInMinuten = (int) organisatieParameterService.getOrganisatieParameter(intakelocatie, OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN);
			nieuweAfspraak.setTot(nieuweAfspraak.getVanaf().plusMinutes(duurAfspraakInMinuten));
		}
		else
		{
			nieuweAfspraak.setVanaf(DateUtil.toLocalDateTime(gekozenVrijSlotZonderKamer.getStartTijd()));
			nieuweAfspraak.setTot(DateUtil.toLocalDateTime(gekozenVrijSlotZonderKamer.getEindTijd()));
		}

		return nieuweAfspraak;
	}
}
