package nl.rivm.screenit.main.controller.algemeen;

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

import nl.rivm.screenit.main.dto.algemeen.BvoStatusDto;
import nl.rivm.screenit.main.dto.algemeen.ClientDto;
import nl.rivm.screenit.main.dto.algemeen.ClientZoekenFilterDto;
import nl.rivm.screenit.main.mappers.algemeen.ClientMapper;
import nl.rivm.screenit.main.service.algemeen.BvoStatusService;
import nl.rivm.screenit.main.service.algemeen.ClientZoekenService;
import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.ProjectClientMapper;
import nl.rivm.screenit.model.algemeen.dto.ProjectClientDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@RequiredArgsConstructor
@RestController
@RequestMapping("/client")
@Tag(name = "Clienten", description = "Beheer en zoek clientgegevens")
public class ClientController
{
	private final ClientZoekenService clientZoekenService;

	private final ClientMapper clientMapper;

	private final LogService logService;

	private final ProjectService projectService;

	private final ProjectClientMapper projectClientMapper;

	private final ICurrentDateSupplier currentDateSupplier;

	private final BvoStatusService bvoStatusService;

	private final ClientService clientService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Zoek clienten", description = "Zoek clienten op basis van filtercriteria.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst van gevonden clienten (ClientDto)"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@PostMapping("/zoeken")
	public ResponseEntity<List<ClientDto>> zoekClienten(
		@RequestBody ClientZoekenFilterDto filter)
	{
		logZoekenGebeurtenis(filter);
		var clienten = clientZoekenService.zoekClienten(filter);
		return ResponseEntity.ok(clienten.stream().map(client -> clientMapper.clientToClientDto(client, clientService)).toList());
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@GetMapping("/{clientId}/actieve-bvos")
	public ResponseEntity<List<Bevolkingsonderzoek>> getActieveBvos(@PathVariable Long clientId)
	{
		var actieveBvos = clientZoekenService.getActieveBvos(clientId);
		return ResponseEntity.ok(actieveBvos);
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@GetMapping("/{clientId}/bvo-status")
	public ResponseEntity<List<BvoStatusDto>> getBvoStatus(@PathVariable Long clientId)
	{
		var client = clientService.getClientById(clientId);
		if (client.isPresent())
		{
			var statussen = bvoStatusService.getBvoStatus(client.get());
			return ResponseEntity.ok(statussen);
		}
		else
		{
			return ResponseEntity.notFound().build();
		}
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

	@GetMapping("/{clientId}/projecten")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_GEGEVENS }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<ProjectClientDto>> getProjectenVoorClient(@PathVariable Long clientId, @RequestParam(required = false) Boolean actief)
	{
		var projecten = projectService.getClientProjecten(clientId);
		var projectDtos = projecten.stream().map(projectClient -> projectClientMapper.projectClientToDto(projectClient, currentDateSupplier))
			.filter(projectClientDto -> projectClientDto.isActief() == actief).toList();
		return ResponseEntity.ok().body(projectDtos);
	}
}
