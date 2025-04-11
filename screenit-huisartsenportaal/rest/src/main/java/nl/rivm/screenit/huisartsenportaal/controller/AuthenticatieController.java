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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.huisartsenportaal.dto.LoginDto;
import nl.rivm.screenit.huisartsenportaal.dto.RegistrerenDto;
import nl.rivm.screenit.huisartsenportaal.dto.TokenDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordAanvragenDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordVergetenDto;
import nl.rivm.screenit.huisartsenportaal.exception.ValidatieException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.service.AuthenticatieService;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.WachtwoordVergetenValidator;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequestMapping("auth")
@RestController
@AllArgsConstructor
@Slf4j
public class AuthenticatieController extends BaseController
{
	private final AuthenticatieService authenticationService;

	private final SynchronisatieService synchronisatieService;

	private final HuisartsService huisartsService;

	private final WachtwoordVergetenValidator wachtwoordVergetenValidator;

	@PostMapping(value = "/inloggen")
	public ResponseEntity<TokenDto> login(@RequestBody LoginDto credentials)
	{
		var loginResponse = authenticationService.inloggen(credentials);
		return ResponseEntity.ok(loginResponse);
	}

	@PostMapping(value = "/registreren")
	public ResponseEntity<TokenDto> registreren(@RequestBody RegistrerenDto credentials)
	{
		var response = authenticationService.registreren(credentials);
		return ResponseEntity.ok(response);
	}

	@PostMapping("/wachtwoord-vergeten")
	public ResponseEntity wachtwoordVergeten(@RequestBody WachtwoordVergetenDto wachtwoordDto)
	{
		var result = new BeanPropertyBindingResult(wachtwoordDto, "wachtwoordDto");
		wachtwoordVergetenValidator.validate(wachtwoordDto, result);
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}

		Huisarts huisarts = huisartsService.getHuisartsWith(wachtwoordDto);
		huisarts = authenticationService.wachtwoordVergeten(huisarts);
		synchronisatieService.syncHuisarts(huisarts);

		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@PostMapping(value = "/wachtwoord-aanvragen")
	public ResponseEntity<TokenDto> wachtwoordAanvragen(@RequestBody WachtwoordAanvragenDto credentials)
	{
		var response = authenticationService.wachtwoordAanvragen(credentials);
		return ResponseEntity.ok(response);
	}

}
