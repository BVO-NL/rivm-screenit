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
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.main.controller.BaseController;
import nl.rivm.screenit.main.dto.algemeen.BezwaarDossierGebeurtenisDto;
import nl.rivm.screenit.main.dto.algemeen.BrpGegevensDto;
import nl.rivm.screenit.main.dto.algemeen.BvoStatusDto;
import nl.rivm.screenit.main.dto.algemeen.ClientContactgegevensDto;
import nl.rivm.screenit.main.dto.algemeen.ClientDto;
import nl.rivm.screenit.main.dto.algemeen.ClientPaspoortDto;
import nl.rivm.screenit.main.dto.algemeen.ClientZoekenFilterDto;
import nl.rivm.screenit.main.dto.algemeen.ScreeningRondeGebeurtenisDto;
import nl.rivm.screenit.main.dto.algemeen.TijdelijkAdresDto;
import nl.rivm.screenit.main.mappers.algemeen.ClientMapper;
import nl.rivm.screenit.main.mappers.algemeen.DossierGebeurtenisWrapper;
import nl.rivm.screenit.main.mappers.algemeen.ScreeningRondeGebeurtenisWrapper;
import nl.rivm.screenit.main.model.DossierGebeurtenis;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.service.DossierService;
import nl.rivm.screenit.main.service.algemeen.BezwaarService;
import nl.rivm.screenit.main.service.algemeen.BvoStatusService;
import nl.rivm.screenit.main.service.algemeen.ClientZoekenService;
import nl.rivm.screenit.main.service.algemeen.ProjectService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.mappers.ProjectClientMapper;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.algemeen.dto.ProjectClientDto;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@AllArgsConstructor
@RestController
@RequestMapping("/client")
@Tag(name = "Clienten", description = "Beheer en zoek clientgegevens")
public class ClientController extends BaseController
{
	private final ClientZoekenService clientZoekenService;

	private final ClientMapper clientMapper;

	private final ClientRepository clientRepository;

	private final LogService logService;

	private final ProjectService projectService;

	private final ProjectClientMapper projectClientMapper;

	private final ICurrentDateSupplier currentDateSupplier;

	private final BvoStatusService bvoStatusService;

	private final DossierService dossierService;

	private final ScreeningRondeGebeurtenisWrapper screeningRondeGebeurtenisWrapper;

	private final ClientContactService clientContactService;

	private final DossierGebeurtenisWrapper dossierGebeurtenisWrapper;

	private final BezwaarService bezwaarService;

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal client op", description = "Haalt de gegevens van een client op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Clientgegevens opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("/{clientId}")
	public ResponseEntity<ClientDto> getClient(@PathVariable Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		return ResponseEntity.ok(clientMapper.clientToClientDto(client, clientService, bezwaarService));
	}

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
		return ResponseEntity.ok(clienten.stream().map(client -> clientMapper.clientToClientDto(client, clientService, bezwaarService)).toList());
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de actieve BVO's op", description = "Haal de BVO's op die actief zijn voor de client")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met bevolkingsonderzoeken"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@GetMapping("/{clientId}/actieve-bvos")
	public ResponseEntity<List<Bevolkingsonderzoek>> getActieveBvos(@PathVariable Long clientId)
	{
		var actieveBvos = clientZoekenService.getActieveBvos(clientId);
		return ResponseEntity.ok(actieveBvos);
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal de status op", description = "Haal de status op van de client voor elke BVO op")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met status objecten"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@GetMapping("/{clientId}/bvo-status")
	public ResponseEntity<List<BvoStatusDto>> getBvoStatus(@PathVariable Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var statussen = bvoStatusService.getBvoStatus(client);
		return ResponseEntity.ok(statussen);
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@GetMapping("/{id}/brp-gegevens")
	public ResponseEntity<BrpGegevensDto> getBrpGegevens(@PathVariable("id") Long clientId)
	{
		var brpGegevens = clientZoekenService.getBrpGegevens(clientId);
		return ResponseEntity.ok(brpGegevens);
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@GetMapping("/{id}/brp-tijdelijk-adres")
	public ResponseEntity<TijdelijkAdresDto> getBrpTijdelijkAdres(@PathVariable("id") Long clientId)
	{
		var tijdelijkAdresDto = clientZoekenService.getBrpTijdelijkAdres(clientId);
		return ResponseEntity.of(Optional.ofNullable(tijdelijkAdresDto));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_GBA_TIJDELIJK_ADRES,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@PostMapping("/{id}/brp-tijdelijk-adres")
	public ResponseEntity<Void> saveBrpTijdelijkAdres(@PathVariable("id") Long clientId, @RequestBody TijdelijkAdresDto tijdelijkAdresDto)
	{
		clientZoekenService.saveBrpTijdelijkAdres(clientId, tijdelijkAdresDto, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.VERWIJDEREN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_GBA_TIJDELIJK_ADRES,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@DeleteMapping("/{id}/brp-tijdelijk-adres")
	public ResponseEntity<Void> deleteBrpTijdelijkAdres(@PathVariable("id") Long clientId)
	{
		clientZoekenService.deleteBrpTijdelijkAdres(clientId, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@GetMapping("{id}/contactgegevens")
	public ResponseEntity<ClientContactgegevensDto> getContactgegevens(@PathVariable Long id)
	{
		return clientRepository.findById(id)
			.map(client ->
			{
				var contactgegevens = clientMapper.clientToClientContactgegevensDto(client, clientContactService);
				clientService.zetDoelgroepenVanClient(client, contactgegevens);
				return contactgegevens;
			})
			.map(ResponseEntity::ok)
			.orElse(ResponseEntity.notFound().build());
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal het clientpaspoort op", description = "Haalt de kerngegevens van een client op voor de horizontale paspoortbalk.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Clientpaspoort opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("{id}/paspoort")
	public ResponseEntity<ClientPaspoortDto> getPaspoort(@PathVariable Long id)
	{
		return clientRepository.findById(id)
			.map(client ->
			{
				var paspoort = clientMapper.clientToClientPaspoortDto(client);
				paspoort.setDoelgroepen(clientService.bepaalDoelgroepenVanClient(client));
				if (ScreenitSession.get().checkPermission(Recht.MEDEWERKER_INZIEN_A_NUMMER, Actie.INZIEN))
				{
					paspoort.setAnummer(client.getPersoon().getAnummer());
				}
				return paspoort;
			})
			.map(ResponseEntity::ok)
			.orElse(ResponseEntity.notFound().build());
	}

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@PutMapping("{id}/contactgegevens")
	public ResponseEntity<Void> slaContactgegevensOp(@RequestBody ClientContactgegevensDto dto, @PathVariable Long id)
	{
		var client = getClientOfGooiNotFoundException(id);
		clientService.slaContactgegevensOp(client, dto, getIngelogdeGebruiker());
		return ResponseEntity.ok().build();
	}

	private void logZoekenGebeurtenis(ClientZoekenFilterDto filter)
	{
		var account = getIngelogdeGebruiker();
		var ingevuldeGeavanceerdeVelden = getIngevuldeGeavanceerdeVelden(filter);
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

	@SecurityConstraint(actie = Actie.INZIEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Haal tijdelijk adres op", description = "Haalt het tijdelijk adres van een client op.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Tijdelijk adres opgehaald"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden")
	})
	@GetMapping("/{clientId}/tijdelijk-adres")
	public ResponseEntity<TijdelijkAdresDto> getTijdelijkAdres(@PathVariable Long clientId)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		var tijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		if (tijdelijkAdres == null)
		{
			return ResponseEntity.ok(null);
		}
		return ResponseEntity.ok(clientMapper.tijdelijkAdresToDto(tijdelijkAdres));
	}

	@SecurityConstraint(actie = Actie.AANPASSEN, checkScope = true, constraint = ShiroConstraint.HasPermission, recht = Recht.MEDEWERKER_CLIENT_GEGEVENS, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	@Operation(summary = "Sla tijdelijk adres op", description = "Slaat het tijdelijk adres van een client op of werkt het bij.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Tijdelijk adres succesvol opgeslagen"),
		@ApiResponse(responseCode = "404", description = "Client niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@PutMapping("/{clientId}/tijdelijk-adres")
	public ResponseEntity<Void> slaTijdelijkAdresOp(@PathVariable Long clientId, @RequestBody TijdelijkAdresDto tijdelijkAdresDto)
	{
		var account = getIngelogdeGebruiker();
		var client = getClientOfGooiNotFoundException(clientId);
		var huidigTijdelijkAdres = client.getPersoon().getTijdelijkAdres();
		TijdelijkAdres tijdelijkAdres;
		if (huidigTijdelijkAdres != null)
		{
			clientMapper.updateTijdelijkAdres(huidigTijdelijkAdres, tijdelijkAdresDto);
			tijdelijkAdres = huidigTijdelijkAdres;
		}
		else
		{
			tijdelijkAdres = clientMapper.dtoToTijdelijkAdres(tijdelijkAdresDto);
		}
		clientContactService.saveTijdelijkAdres(account, client, tijdelijkAdres);
		return ResponseEntity.ok().build();
	}

	@Operation(summary = "Haal de projecten voor de client op", description = "Haal de projecten op waar de client actief in is")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met projecten"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
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

	@Operation(summary = "Haal de screeningronde gebeurtenissen van de client op", description = "Haal de gebeurtenissen voor de client op, gefilterd op gebeurtenis type")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met screeningronde gebeurtenissen"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@GetMapping("/{clientId}/screeningronde-gebeurtenissen/{type}")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_GEGEVENS }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<ScreeningRondeGebeurtenisDto>> getScreeningrondeGebeurtenissen(@PathVariable Long clientId, @PathVariable String type)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (type.equals("algemene-brieven"))
		{
			var gebeurtenissen = dossierService.getAlgemeneBriefGebeurtenissen(client);
			var screeningRondeGebeurtenisDtos = gebeurtenissen.stream()
				.sorted(Comparator.comparing(ScreeningRondeGebeurtenis::getDatum).reversed())
				.map(screeningRondeGebeurtenisWrapper::screeningRondeGebeurtenisNaarDto)
				.toList();
			return ResponseEntity.ok().body(screeningRondeGebeurtenisDtos);
		}
		return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).build();
	}

	@Operation(summary = "Haal de dossier gebeurtenissen van de client op", description = "Haal de dossier gebeurtenissen voor de client op, gefilterd op type")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Lijst met dossier gebeurtenissen"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@GetMapping("/{clientId}/dossier-gebeurtenissen/{type}")
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = { Recht.MEDEWERKER_CLIENT_GEGEVENS }, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<BezwaarDossierGebeurtenisDto>> getDossierGebeurtenissen(@PathVariable Long clientId, @PathVariable String type)
	{
		var client = getClientOfGooiNotFoundException(clientId);
		if (type.equals("bezwaar"))
		{
			var gebeurtenissen = dossierService.getBezwaarGebeurtenissen(client);
			var dossierGebeurtenisDtos = gebeurtenissen.stream()
				.sorted(Comparator.comparing(DossierGebeurtenis::getTijd).reversed())
				.map(dossierGebeurtenisWrapper::bezwaarDossierGebeurtenisNaarDto)
				.toList();
			return ResponseEntity.ok().body(dossierGebeurtenisDtos);
		}
		return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).build();
	}
}
