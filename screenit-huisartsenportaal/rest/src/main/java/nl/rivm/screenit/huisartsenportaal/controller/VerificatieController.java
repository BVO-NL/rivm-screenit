package nl.rivm.screenit.huisartsenportaal.controller;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import nl.rivm.screenit.huisartsenportaal.dto.VerificatieLocatieDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerificatieStatusDto;
import nl.rivm.screenit.huisartsenportaal.exception.ValidatieException;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.LocatieVerificatieService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;

@RestController
@RequestMapping("verificatie")
public class VerificatieController extends BaseController
{
	@Autowired
	private LocatieVerificatieService verificatieService;

	@Autowired
	private LocatieService locatieService;

	@GetMapping("/locaties")
	public List<VerificatieLocatieDto> getLocaties()
	{
		return verificatieService.getTeVerifierenLocaties(getIngelogdeHuisarts());
	}

	@PostMapping("/verifieerLocatie")
	public ResponseEntity verifieerHuisartsLocatieCode(@Valid @RequestBody VerificatieLocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}
		VerificatieStatusDto verificatieStatus = verificatieService.verifieerLocatie(locatieDto);
		if (verificatieStatus.getSucces())
		{
			return ResponseEntity.ok(verificatieStatus);
		}
		else
		{
			return ResponseEntity.badRequest().body(verificatieStatus);
		}
	}

	@PostMapping("/herzendVerificatieCode")
	public ResponseEntity herzendVerificatieCode(@Valid @RequestBody VerificatieLocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}
		locatieService.herzendVerificatieMail(getIngelogdeHuisarts(), locatieDto);
		return ResponseEntity.ok().build();
	}
}
