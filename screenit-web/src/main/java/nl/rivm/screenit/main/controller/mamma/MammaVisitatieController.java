package nl.rivm.screenit.main.controller.mamma;

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

import java.util.List;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.PagineringRequestDto;
import nl.rivm.screenit.main.dto.PagineringResponseDto;
import nl.rivm.screenit.main.dto.mamma.visitatie.MammaVisitatieDto;
import nl.rivm.screenit.main.dto.mamma.visitatie.MammaVisitatieRequestDto;
import nl.rivm.screenit.main.dto.mamma.visitatie.MammaVisitatieResponseDto;
import nl.rivm.screenit.main.dto.mamma.visitatie.MammaVisitatieWerklijstFilterDto;
import nl.rivm.screenit.main.dto.mamma.visitatie.MammaVisitatielijstRequestDto;
import nl.rivm.screenit.main.dto.mamma.visitatie.MammaVisitatielijstResponseDto;
import nl.rivm.screenit.main.mappers.mamma.MammaVisitatieMapper;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.service.mamma.MammaVisitatieService;
import nl.rivm.screenit.main.util.PagineringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.wicketstuff.shiro.ShiroConstraint;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/mamma/visitatie")
@Tag(name = "Mamma visitatie", description = "Zoeken, beheren en genereren van visitaties binnen mamma")
public class MammaVisitatieController
{
	private final MammaVisitatieService visitatieService;

	private final MammaVisitatieMapper mammaVisitatieMapper;

	private final MammaKwaliteitscontroleService kwaliteitscontroleService;

	@PostMapping(value = "/zoeken")
	@Operation(summary = "Zoek visitaties", description = "Zoekt visitaties op basis van filtercriteria.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Visitaties succesvol gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<PagineringResponseDto<List<MammaVisitatieDto>>> getVisitaties(
		@Parameter(description = "Zoekfilter en paginering voor visitaties") @RequestBody PagineringRequestDto<MammaVisitatieWerklijstFilterDto> body)
	{
		var paginering = PagineringUtil.maakPageVanRequest(body.getPaginering(), body.getSortering());
		var filter = body.getData();
		var visitaties = visitatieService.zoekVisitaties(filter, paginering).stream()
			.map(mammaVisitatieMapper::mammaVisitatieToDto).toList();
		var totaal = (int) visitatieService.countVisitaties(filter);

		var response = new PagineringResponseDto<List<MammaVisitatieDto>>();
		response.setData(visitaties);
		response.setPaginering(body.getPaginering());
		response.getPaginering().setTotaal(totaal);
		return ResponseEntity.ok(response);
	}

	@GetMapping(value = "/omschrijving")
	@Operation(summary = "Zoek visitaties op omschrijving", description = "Geeft visitaties terug met een omschrijving die past bij de opgegeven waarde.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Visitaties succesvol opgehaald"),
		@ApiResponse(responseCode = "400", description = "De opgegeven waarde is ongeldig"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<List<MammaVisitatieDto>> getUniekOmschrijving(@Parameter(description = "Omschrijving waarop gezocht wordt") @RequestParam String waarde)
	{
		if (StringUtils.isBlank(waarde))
		{
			return ResponseEntity.badRequest().build();
		}
		var visitaties = visitatieService.getByOmschrijving(waarde).stream()
			.map(mammaVisitatieMapper::mammaVisitatieToDto).toList();

		return ResponseEntity.ok(visitaties);
	}

	@PostMapping()
	@Operation(summary = "Maak een visitatie aan", description = "Maakt een nieuwe visitatie aan met metadata en optionele bijlagen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Visitatie succesvol aangemaakt"),
		@ApiResponse(responseCode = "413", description = "Bestand is te groot om te uploaden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<MammaVisitatieResponseDto> maakVisitatie(
		@Parameter(description = "Metadata van de visitatie") @RequestPart("metadata") MammaVisitatieRequestDto dto,
		@Parameter(description = "Optionele aanvullende bestanden") @RequestParam(required = false) Map<String, MultipartFile> bestanden,
		@Parameter(description = "Optionele rapportage") @RequestPart(value = "rapportage", required = false) MultipartFile rapportage,
		@Parameter(description = "Optionele vragenlijst") @RequestPart(value = "vragenlijst", required = false) MultipartFile vragenlijst)
	{
		var visitatieResponseDto = kwaliteitscontroleService.maakOfBewerkVisitatie(dto, bestanden, rapportage, vragenlijst,
			ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(visitatieResponseDto);
	}

	@PostMapping("/insteltechniek/genereren")
	@Operation(summary = "Genereer een insteltechniek-visitatielijst", description = "Genereert een visitatielijst voor insteltechniek op basis van metadata en een optioneel bestand.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Visitatielijst succesvol gegenereerd"),
		@ApiResponse(responseCode = "413", description = "Bestand is te groot om te uploaden"),
		@ApiResponse(responseCode = "500", description = "Genereren van de visitatielijst is mislukt")
	})
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.RIVM })
	ResponseEntity<MammaVisitatielijstResponseDto> genereerInsteltechniekVisitatielijst(
		@Parameter(description = "Metadata voor de visitatielijst") @RequestPart("metadata") MammaVisitatielijstRequestDto dto,
		@Parameter(description = "Optioneel bronbestand voor de visitatielijst") @RequestPart(value = "bestand", required = false) MultipartFile bestand,
		@Parameter(description = "Voer de generatie uit als controle zonder definitief resultaat") @RequestParam boolean dryRun)
	{
		try
		{
			var visitatieResponseDto = kwaliteitscontroleService.genereerInsteltechniekVisitatielijst(dto, bestand, dryRun,
				ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
			return ResponseEntity.ok(visitatieResponseDto);
		}
		catch (Exception e)
		{
			LOG.error("Fout bij genereren visitatielijst", e);
			return ResponseEntity.internalServerError().build();
		}
	}

	@PutMapping("{id}")
	@Operation(summary = "Werk een visitatie bij", description = "Werkt een bestaande visitatie bij met metadata en optionele bijlagen.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Visitatie succesvol bijgewerkt"),
		@ApiResponse(responseCode = "400", description = "Id in pad en metadata komen niet overeen"),
		@ApiResponse(responseCode = "413", description = "Bestand is te groot om te uploaden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<MammaVisitatieResponseDto> updateVisitatie(
		@Parameter(description = "Id van de visitatie") @PathVariable long id,
		@Parameter(description = "Aangepaste metadata van de visitatie") @RequestPart("metadata") MammaVisitatieRequestDto dto,
		@Parameter(description = "Optionele aanvullende bestanden") @RequestParam(required = false) Map<String, MultipartFile> bestanden,
		@Parameter(description = "Optionele rapportage") @RequestPart(value = "rapportage", required = false) MultipartFile rapportage,
		@Parameter(description = "Optionele vragenlijst") @RequestPart(value = "vragenlijst", required = false) MultipartFile vragenlijst)

	{
		if (dto.getId() != null && !dto.getId().equals(id))
		{
			return ResponseEntity.badRequest().build();
		}
		dto.setId(id);
		var visitatieResponseDto = kwaliteitscontroleService.maakOfBewerkVisitatie(dto, bestanden, rapportage, vragenlijst,
			ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(visitatieResponseDto);
	}

	@DeleteMapping("{id}")
	@Operation(summary = "Verwijder een visitatie", description = "Verwijdert een visitatie op basis van het opgegeven id.")
	@ApiResponses(value = {
		@ApiResponse(responseCode = "200", description = "Visitatie succesvol verwijderd"),
		@ApiResponse(responseCode = "404", description = "Visitatie niet gevonden"),
		@ApiResponse(responseCode = "500", description = "Onverwachte fout opgetreden")
	})
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<Void> verwijderVisitatie(@Parameter(description = "Id van de te verwijderen visitatie") @PathVariable long id)
	{
		var visitatie = visitatieService.getById(id);
		if (visitatie == null)
		{
			return ResponseEntity.notFound().build();
		}
		kwaliteitscontroleService.deleteVisitatie(visitatie);

		return ResponseEntity.ok().build();
	}
}
