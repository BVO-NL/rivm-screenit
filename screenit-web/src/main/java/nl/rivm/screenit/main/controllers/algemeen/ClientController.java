package nl.rivm.screenit.main.controllers.algemeen;

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

import nl.rivm.screenit.main.dto.algemeen.ClientDto;
import nl.rivm.screenit.main.dto.algemeen.ClientZoekenFilterDto;
import nl.rivm.screenit.main.mappers.algemeen.ClientMapper;
import nl.rivm.screenit.main.service.algemeen.ClientZoekenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@RequiredArgsConstructor
@RestController
@RequestMapping("/api/client")
public class ClientController
{
	private final ClientZoekenService clientZoekenService;

	private final ClientMapper clientMapper;

	private final LogService logService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@PostMapping("/zoeken")
	public ResponseEntity<List<ClientDto>> zoekClienten(@RequestBody ClientZoekenFilterDto filter)
	{
		logZoekenGebeurtenis(filter);
		var clienten = clientZoekenService.zoekClienten(filter);
		return ResponseEntity.ok(clienten.stream().map(clientMapper::clientToClientDto).toList());
	}

	private void logZoekenGebeurtenis(ClientZoekenFilterDto filter)
	{
		var account = ScreenitSession.get().getIngelogdAccount();
		List<String> ingevuldeGeavanceerdeVelden = getIngevuldeGeavanceerdeVelden(filter);
		if (!ingevuldeGeavanceerdeVelden.isEmpty())
		{
			logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_CLIENT, account, "Uitgebreid zoeken. Gezocht op " + String.join(", ", getIngevuldeVelden(filter)));
		}
		else if (StringUtils.isNotBlank(filter.getBsn()))
		{
			logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_CLIENT, account, "Gezocht op bsn: " + filter.getBsn());
		}
		else if (StringUtils.isNotBlank(filter.getPostcode()) && filter.getHuisnummer() != null)
		{
			logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_CLIENT, account,
				"Gezocht op postcode + huisnummer: " + filter.getPostcode() + " + " + filter.getHuisnummer());
		}
		else if (StringUtils.isNotBlank(filter.getBriefkenmerk()))
		{
			logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_CLIENT, account, "Gezocht op briefkenmerk: " + filter.getBriefkenmerk());
		}
		else
		{
			var logEvent = new LogEvent("Gezocht op alleen geboortedatum.");
			logEvent.setLevel(Level.ERROR);
			logService.logGebeurtenis(LogGebeurtenis.ZOEKEN_CLIENT, logEvent, account);
		}
	}

	private List<String> getIngevuldeVelden(final ClientZoekenFilterDto filter)
	{
		var velden = getIngevuldeGeavanceerdeVelden(filter);
		if (StringUtils.isNotBlank(filter.getBsn()))
		{
			velden.add("bsn: " + filter.getBsn());
		}
		if (StringUtils.isNotBlank(filter.getPostcode()) && filter.getHuisnummer() != null)
		{
			velden.add("postcode + huisnummer: " + filter.getPostcode() + " + " + filter.getHuisnummer());
		}
		if (StringUtils.isNotBlank(filter.getBriefkenmerk()))
		{
			velden.add("briefkenmerk: " + filter.getBriefkenmerk());
		}
		return velden;
	}

	private List<String> getIngevuldeGeavanceerdeVelden(ClientZoekenFilterDto filter)
	{
		var velden = new ArrayList<String>();
		if (filter.getBkUitnodigingsnummer() != null)
		{
			velden.add("BK uitnodigingsnummer: " + filter.getBkUitnodigingsnummer());
		}
		if (StringUtils.isNotBlank(filter.getBmhkMonsterId()))
		{
			velden.add("BMHK monster-ID: " + filter.getBmhkMonsterId());
		}
		if (filter.getBmhkUitnodigingsId() != null)
		{
			velden.add("BMHK uitnodigings-ID: " + filter.getBmhkUitnodigingsId());
		}
		if (StringUtils.isNotBlank(filter.getDkBarcode()))
		{
			velden.add("DK barcode: " + filter.getDkBarcode());
		}
		if (filter.getDkUitnodigingsId() != null)
		{
			velden.add("DK uitnodigings-ID: " + filter.getDkUitnodigingsId());
		}
		if (StringUtils.isNotBlank(filter.getAnummer()))
		{
			velden.add("A-nummer: " + filter.getAnummer());
		}
		if (StringUtils.isNotBlank(filter.getMobielnummer()))
		{
			velden.add("mobielnummer: " + filter.getMobielnummer());
		}
		if (StringUtils.isNotBlank(filter.getEmailadres()))
		{
			velden.add("emailadres: " + filter.getEmailadres());
		}
		return velden;
	}
}
