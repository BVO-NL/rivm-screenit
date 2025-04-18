package nl.rivm.screenit.clientportaal.controllers;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.clientportaal.mappers.HuisartsMapper;
import nl.rivm.screenit.clientportaal.model.HuisartsDto;
import nl.rivm.screenit.clientportaal.model.HuisartsZoekDto;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.EnovationHuisarts_;
import nl.rivm.screenit.service.ClientContactService;
import nl.rivm.screenit.service.EnovationHuisartsService;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("huisarts")
@AllArgsConstructor
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class HuisartsController extends AbstractController
{
	private final ClientContactService clientContactService;

	private final EnovationHuisartsService huisartsService;

	private final HuisartsMapper huisartsMapper;

	private static final int MAX_AANTAL_HUISARTSEN_PER_CALL = 10;

	@PostMapping
	public ResponseEntity<List<HuisartsDto>> getHuisartsen(Authentication authentication, @RequestParam Integer paginaNummer, @RequestBody HuisartsZoekDto huisartsZoekobject)
	{
		var client = getClient(authentication);

		if (clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN)
			|| clientContactService.availableActiesBevatBenodigdeActie(client, ClientContactActieType.COLON_HUISARTS_WIJZIGEN))
		{
			var enovationHuisartsen = huisartsService.zoekHuisartsen(
				huisartsMapper.zoekDtoToHuisarts(huisartsZoekobject), PageRequest.of(paginaNummer, MAX_AANTAL_HUISARTSEN_PER_CALL, Sort.by(EnovationHuisarts_.ACHTERNAAM)));

			return ResponseEntity.ok(huisartsMapper.huisartsenToDtos(enovationHuisartsen));
		}
		return createForbiddenResponse();
	}

}
