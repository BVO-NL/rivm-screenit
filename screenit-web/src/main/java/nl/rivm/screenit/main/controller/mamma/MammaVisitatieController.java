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

import nl.rivm.screenit.main.mappers.mamma.MammaVisitatieMapper;
import nl.rivm.screenit.main.model.dto.PagineringRequestDto;
import nl.rivm.screenit.main.model.dto.PagineringResponseDto;
import nl.rivm.screenit.main.model.mamma.dto.MammaVisitatieDto;
import nl.rivm.screenit.main.model.mamma.dto.MammaVisitatieRequestDto;
import nl.rivm.screenit.main.model.mamma.dto.MammaVisitatieResponseDto;
import nl.rivm.screenit.main.model.mamma.dto.MammaVisitatieWerklijstFilterDto;
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

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/mamma/visitatie")
public class MammaVisitatieController
{
	private final MammaVisitatieService visitatieService;

	private final MammaVisitatieMapper mammaVisitatieMapper;

	private final MammaKwaliteitscontroleService kwaliteitscontroleService;

	@PostMapping(value = "/zoeken")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<PagineringResponseDto<List<MammaVisitatieDto>>> getVisitaties(@RequestBody PagineringRequestDto<MammaVisitatieWerklijstFilterDto> body)
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
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<List<MammaVisitatieDto>> getUniekOmschrijving(@RequestParam("waarde") String waarde)
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
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<MammaVisitatieResponseDto> maakVisitatie(@RequestPart("metadata") MammaVisitatieRequestDto dto,
		@RequestParam(required = false) Map<String, MultipartFile> bestanden,
		@RequestPart(value = "rapportage", required = false) MultipartFile rapportage, @RequestPart(value = "vragenlijst", required = false) MultipartFile vragenlijst)
	{
		var visitatieResponseDto = kwaliteitscontroleService.maakOfBewerkVisitatie(dto, bestanden, rapportage, vragenlijst, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(visitatieResponseDto);
	}

	@PutMapping("{id}")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<MammaVisitatieResponseDto> updateVisitatie(@RequestPart("metadata") MammaVisitatieRequestDto dto,
		@RequestParam(required = false) Map<String, MultipartFile> bestanden,
		@RequestPart(value = "rapportage", required = false) MultipartFile rapportage, @RequestPart(value = "vragenlijst", required = false) MultipartFile vragenlijst)

	{
		var visitatieResponseDto = kwaliteitscontroleService.maakOfBewerkVisitatie(dto, bestanden, rapportage, vragenlijst, ScreenitSession.get().getIngelogdeOrganisatieMedewerker());
		return ResponseEntity.ok(visitatieResponseDto);
	}

	@DeleteMapping("{id}")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_VISITATIE, Recht.MEDEWERKER_VISITATIE_INSTELTECHNIEK },
		organisatieTypeScopes = { OrganisatieType.KWALITEITSPLATFORM, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	ResponseEntity<Void> verwijderVisitatie(@PathVariable long id)
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
