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

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.algemeen.AfspraakActie;
import nl.rivm.screenit.main.dto.algemeen.AfspraakDto;
import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.mappers.mamma.MammaAfspraakMapper;
import nl.rivm.screenit.main.service.mamma.MammaClientAfspraakService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class MammaClientAfspraakServiceImpl implements MammaClientAfspraakService
{
	private final ClientService clientService;

	private final MammaBaseAfspraakService baseAfspraakService;

	private final MammaBaseDossierService baseDossierService;

	private final MammaAfspraakMapper afspraakMapper;

	@Override
	@Transactional(readOnly = true)
	public List<AfspraakDto> getAfspraken(Long clientId)
	{
		var client = clientService.getClientById(clientId).orElseThrow(() -> new EntityNietGevondenException("Client", clientId));
		var dossier = client.getMammaDossier();
		if (dossier == null || dossier.getLaatsteScreeningRonde() == null)
		{
			return List.of();
		}

		var afspraak = dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null
			? dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak()
			: null;
		return afspraak != null && afspraak.getStatus() == MammaAfspraakStatus.GEPLAND
			? List.of(afspraakMapper.afspraakNaarDto(afspraak, baseAfspraakService.getMammaStandplaatsLocatieAfspraak(afspraak)))
			: List.of();
	}

	@Override
	@Transactional(readOnly = true)
	public List<AfspraakActie> getAfspraakActies(Long clientId)
	{
		var client = clientService.getClientById(clientId).orElseThrow(() -> new EntityNietGevondenException("Client", clientId));
		return getAfspraakActies(client);
	}

	private List<AfspraakActie> getAfspraakActies(Client client)
	{
		List<AfspraakActie> acties = new ArrayList<>();
		if (client.getMammaDossier() == null || client.getMammaDossier().getLaatsteScreeningRonde() == null)
		{
			return acties;
		}

		if (baseDossierService.isAfspraakMakenMogelijk(client.getMammaDossier(), false, false))
		{
			acties.add(AfspraakActie.MAKEN);
		}

		return acties;
	}
}
