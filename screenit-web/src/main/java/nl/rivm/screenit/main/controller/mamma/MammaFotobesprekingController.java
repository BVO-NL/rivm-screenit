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

import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaFotobesprekingDto;
import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaFotobesprekingOnderzoekFilterOptiesDto;
import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaFotobesprekingOnderzoekRequestDto;
import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaFotobesprekingOnderzoekResponseDto;
import nl.rivm.screenit.main.mappers.mamma.MammaFotobesprekingMapper;
import nl.rivm.screenit.main.service.mamma.MammaFotobesprekingService;
import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;
import nl.rivm.screenit.service.ClientService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/mamma/fotobespreking")
public class MammaFotobesprekingController
{
	private final MammaFotobesprekingService fotobesprekingService;

	private final MammaFotobesprekingMapper fotobesprekingMapper;

	private final MammaKwaliteitscontroleService kwaliteitscontroleService;

	private final ClientService clientService;

	@GetMapping("/{id}")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_FOTOBESPREKING },
		organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<MammaFotobesprekingDto> getFotobespreking(@PathVariable long id)
	{
		var fotobespreking = fotobesprekingService.getFotobespreking(id);
		if (fotobespreking == null)
		{
			LOG.warn("Fotobespreking met id {} niet gevonden", id);
			return ResponseEntity.notFound().build();
		}
		return ResponseEntity.ok(fotobesprekingMapper.mammaFotobesprekingToDto(fotobespreking));
	}

	@PutMapping("/{id}/onderzoeken")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_FOTOBESPREKING },
		organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<MammaFotobesprekingOnderzoekResponseDto> voegOnderzoekenToeAanFotobespreking(@PathVariable long id,
		@RequestBody MammaFotobesprekingOnderzoekRequestDto requestDto)
	{
		var fotobespreking = fotobesprekingService.getFotobespreking(id);
		if (fotobespreking == null)
		{
			LOG.warn("Fotobespreking met id {} niet gevonden", id);
			return ResponseEntity.notFound().build();
		}

		var response = new MammaFotobesprekingOnderzoekResponseDto(requestDto.getClientIds()
			.stream()
			.map(clientService::getClientById)
			.filter(Optional::isPresent)
			.map(Optional::get)
			.map(client -> kwaliteitscontroleService.addFotobesprekingOnderzoek(fotobespreking, client))
			.filter(Objects::nonNull)
			.toList());

		return ResponseEntity.ok(response);

	}

	@GetMapping("/filter-opties")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_FOTOBESPREKING },
		organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<MammaFotobesprekingOnderzoekFilterOptiesDto> getFotobesprekingFilterOpties()
	{
		var opties = new MammaFotobesprekingOnderzoekFilterOptiesDto();
		opties.setRedenFotobesprekingDoorMbber(Arrays.asList(MammaOnderzoekRedenFotobespreking.values()));
		opties.setRedenFotobesprekingMetMbber(Arrays.asList(MammaLezingRedenenFotobesprekingMbber.values()));
		opties.setRedenFotobesprekingDoorRadioloog(Arrays.asList(MammaLezingRedenenFotobesprekingRadioloog.values()));
		opties.setRedenDoorverwijzing(Stream.of(MammaLaesieType.values()).filter(l -> !l.name().contains("LEGACY")).toList());
		opties.setFollowUp(Arrays.asList(MammaFollowUpConclusieStatus.values()));
		return ResponseEntity.ok(opties);
	}
}
