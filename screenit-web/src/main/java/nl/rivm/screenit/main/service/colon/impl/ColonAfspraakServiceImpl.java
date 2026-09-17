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

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.algemeen.AfspraakActie;
import nl.rivm.screenit.main.dto.algemeen.AfspraakDto;
import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.mappers.colon.ColonAfspraakMapper;
import nl.rivm.screenit.main.service.colon.ColonAfspraakService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.repository.colon.ColonIntakeAfspraakRepository;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.colon.ColonBaseAfspraakService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class ColonAfspraakServiceImpl implements ColonAfspraakService
{
	private final ColonBaseAfspraakService baseAfspraakService;

	private final ColonIntakeAfspraakRepository intakeAfspraakRepository;

	private final OrganisatieParameterService organisatieParameterService;

	private final ColonAfspraakMapper afspraakMapper;

	@Override
	@Transactional(readOnly = true)
	public List<AfspraakDto> getAfspraken(Client client)
	{
		return intakeAfspraakRepository.findAllByClientIdAndStatusOrderByVanafDesc(client.getId(), ColonAfspraakStatus.GEPLAND).stream()
			.map(afspraak -> afspraakMapper.colonAfspraakToAfspraakDto(afspraak, organisatieParameterService))
			.toList();
	}

	@Override
	@Transactional(readOnly = true)
	public List<AfspraakActie> getAfspraakActies(Client client, Long afspraakId)
	{
		var colonDossier = client.getColonDossier();
		var afspraak = getAfspraakOfNull(client, afspraakId);
		var heeftOnafgerondeVerwijzingOmMedischeRedenen = baseAfspraakService.heeftOnafgerondeVerwijzingOmMedischeRedenen(afspraak);

		List<AfspraakActie> acties = new ArrayList<>();
		if (colonDossier != null && colonDossier.getLaatsteScreeningRonde() != null && (afspraak == null || heeftOnafgerondeVerwijzingOmMedischeRedenen)
			&& baseAfspraakService.magNieuweAfspraakMaken(client))
		{
			acties.add(AfspraakActie.MAKEN);
		}

		if (afspraak != null && !heeftOnafgerondeVerwijzingOmMedischeRedenen)
		{
			acties.add(AfspraakActie.VERPLAATSEN);
			acties.add(AfspraakActie.AFZEGGEN);
		}
		return acties;
	}

	private ColonIntakeAfspraak getAfspraakOfNull(Client client, Long afspraakId)
	{
		if (afspraakId == null)
		{
			return null;
		}
		var afspraak = intakeAfspraakRepository.findById(afspraakId).orElseThrow(() -> new EntityNietGevondenException("Intakeafspraak", afspraakId));
		if (!client.getId().equals(afspraak.getClient().getId()))
		{
			throw new EntityNietGevondenException("Intakeafspraak", afspraakId);
		}
		return afspraak;
	}
}
