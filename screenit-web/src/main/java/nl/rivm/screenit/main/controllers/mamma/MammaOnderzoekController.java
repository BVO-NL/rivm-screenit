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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.dto.PagineringRequestDto;
import nl.rivm.screenit.main.dto.PagineringResponseDto;
import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaFotobesprekingOnderzoekFilterDto;
import nl.rivm.screenit.main.dto.mamma.fotobespreking.MammaOnderzoekDto;
import nl.rivm.screenit.main.mappers.mamma.MammaOnderzoekMapper;
import nl.rivm.screenit.main.service.mamma.MammaOnderzoekService;
import nl.rivm.screenit.main.util.PagineringUtil;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@AllArgsConstructor
@RestController
@RequestMapping("/api/mamma/onderzoek")
public class MammaOnderzoekController
{
	private final MammaOnderzoekMapper onderzoekMapper;

	private final MammaOnderzoekService onderzoekService;

	@PostMapping("/zoeken")
	@SecurityConstraint(
		constraint = ShiroConstraint.HasPermission,
		bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA },
		recht = { Recht.MEDEWERKER_FOTOBESPREKING },
		organisatieTypeScopes = { OrganisatieType.BEOORDELINGSEENHEID, OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
	public ResponseEntity<PagineringResponseDto<List<MammaOnderzoekDto>>> zoekFotobesprekingOnderzoeken(
		@RequestBody PagineringRequestDto<MammaFotobesprekingOnderzoekFilterDto> body)
	{
		var paginering = PagineringUtil.maakPageVanRequest(body.getPaginering(), body.getSortering());
		var filter = body.getData();
		var onderzoeken = onderzoekService.zoekOnderzoeken(filter, paginering).stream().map(onderzoekMapper::mammaOnderzoekToDto).toList();
		var totaal = (int) onderzoekService.countOnderzoeken(filter);

		var response = new PagineringResponseDto<List<MammaOnderzoekDto>>();
		response.setData(onderzoeken);
		response.setPaginering(body.getPaginering());
		response.getPaginering().setTotaal(totaal);
		return ResponseEntity.ok(response);
	}
}
