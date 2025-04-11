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

import nl.rivm.screenit.huisartsenportaal.dto.LocatieDto;
import nl.rivm.screenit.huisartsenportaal.exception.ValidatieException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Locatie;
import nl.rivm.screenit.huisartsenportaal.service.LocatieService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.LocatieValidator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;

@RestController
@RequestMapping("locatie")
public class LocatieController extends BaseController
{
	@Autowired
	private LocatieValidator locatieValidator;

	@Autowired
	private LocatieService locatieService;

	@Autowired
	private SynchronisatieService syncService;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{

		binder.addValidators(locatieValidator);
	}

	@GetMapping
	public ResponseEntity getLocaties()
	{
		Huisarts arts = getIngelogdeHuisarts();
		if (arts != null)
		{
			var locaties = locatieService.getAllLocatiesFromHuisartsInDto(arts);
			return ResponseEntity.ok(locaties);
		}
		return ResponseEntity.badRequest().body("Er is iets misgegaan.");
	}

	@PostMapping
	public ResponseEntity saveLocatie(@Valid @RequestBody LocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}
		var arts = getIngelogdeHuisarts();
		var allLocatiesFromHuisartsInDto = locatieService.getAllLocatiesFromHuisartsInDto(arts);
		var locatie = locatieService.updateAndGetLocatie(arts, locatieDto);
		locatieService.nietVerstuurdeLabformulierenVerwijderen(locatieDto);
		syncService.syncLocatie(arts, locatie, locatieDto.getHerzendVerificatieMail());
		return ResponseEntity.ok(allLocatiesFromHuisartsInDto);
	}

	@PutMapping
	public ResponseEntity putLocatie(@Valid @RequestBody LocatieDto locatieDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}

		Huisarts arts = getIngelogdeHuisarts();
		Locatie locatie = locatieService.updateAndGetLocatie(arts, locatieDto);
		locatieService.nietVerstuurdeLabformulierenVerwijderen(locatieDto);
		syncService.syncLocatie(arts, locatie, locatieDto.getHerzendVerificatieMail());
		return ResponseEntity.ok(locatieService.getLocatieDto(locatie));
	}
}
