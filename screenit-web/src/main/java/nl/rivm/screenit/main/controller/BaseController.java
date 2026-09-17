package nl.rivm.screenit.main.controller;

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

import nl.rivm.screenit.main.exception.EntityNietGevondenException;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.service.ClientService;

import org.springframework.beans.factory.annotation.Autowired;

public class BaseController
{
	@Autowired
	protected ClientService clientService;

	protected OrganisatieMedewerker getIngelogdeGebruiker()
	{
		return ScreenitSession.get().getIngelogdeOrganisatieMedewerker();
	}

	protected Client getClientOfGooiNotFoundException(Long clientId)
	{
		return clientService.getClientById(clientId).orElseThrow(() -> new EntityNietGevondenException("Client", clientId));
	}
}
