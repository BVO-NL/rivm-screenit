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

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.MedewerkerDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordWijzigenDto;
import nl.rivm.screenit.huisartsenportaal.exception.ValidatieException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;
import nl.rivm.screenit.huisartsenportaal.repository.OvereenkomstRepository;
import nl.rivm.screenit.huisartsenportaal.service.AuthenticatieService;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;
import nl.rivm.screenit.huisartsenportaal.service.SynchronisatieService;
import nl.rivm.screenit.huisartsenportaal.validator.GebruikersnaamValidator;
import nl.rivm.screenit.huisartsenportaal.validator.HuisartsValidator;
import nl.rivm.screenit.huisartsenportaal.validator.WachtwoordWijzigenValidator;

import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;

@RestController
@RequestMapping("huisarts")
@RequiredArgsConstructor
public class HuisartsController extends BaseController
{

	private final GebruikersnaamValidator gebruikersnaamValidator;

	private final HuisartsValidator huisartsValidator;

	private final HuisartsService huisartsService;

	private final SynchronisatieService syncService;

	private final OvereenkomstRepository overeenkomstRepository;

	private final ModelMapper modelMapper;

	private final WachtwoordWijzigenValidator wachtwoordWijzigenValidator;

	private final AuthenticatieService authenticationService;

	@GetMapping
	public ResponseEntity<HuisartsDto> getHuisarts()
	{
		var arts = getIngelogdeHuisarts();
		if (arts != null)
		{
			var dto = new HuisartsDto();
			modelMapper.map(getIngelogdeHuisarts(), dto);
			dto.setUsername(arts.getGebruikersnaam());
			return new ResponseEntity<>(dto, HttpStatus.OK);
		}
		else
		{
			return new ResponseEntity<>(HttpStatus.UNAUTHORIZED);
		}
	}

	@PutMapping(value = "/controle")
	public ResponseEntity controleHuisarts(@Valid @RequestBody HuisartsDto huisartsDto)
	{
		var result = new BeanPropertyBindingResult(huisartsDto, "huisartsDto");
		gebruikersnaamValidator.validate(huisartsDto, result);
		huisartsValidator.validate(huisartsDto, result);
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}
		return ResponseEntity.ok().body(huisartsDto);
	}

	@PutMapping
	public ResponseEntity updateHuisarts(@Valid @RequestBody HuisartsDto huisartsDto)
	{
		var result = new BeanPropertyBindingResult(huisartsDto, "huisartsDto");
		gebruikersnaamValidator.validate(huisartsDto, result);
		huisartsValidator.validate(huisartsDto, result);
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}

		var huisarts = getIngelogdeHuisarts();
		huisarts = huisartsService.updateAndGetHuisarts(huisartsDto, huisarts);
		syncService.syncHuisarts(huisarts);

		var dto = new HuisartsDto();
		modelMapper.map(getIngelogdeHuisarts(), dto);
		dto.setUsername(getIngelogdeHuisarts().getGebruikersnaam());
		return new ResponseEntity<>(dto, HttpStatus.OK);
	}

	@GetMapping(value = "/currentuser")
	public ResponseEntity<MedewerkerDto> getCurrentUser()
	{
		var huisarts = getIngelogdeHuisarts();
		if (huisarts != null)
		{
			return new ResponseEntity<>(getMedewerkerDtoFrom(huisarts), HttpStatus.OK);
		}
		return new ResponseEntity<>(HttpStatus.NOT_FOUND);
	}

	@PostMapping("/wachtwoord-wijzigen")
	public ResponseEntity wachtwoordWijzigen(@RequestBody WachtwoordWijzigenDto wachtwoordDto)
	{
		var result = new BeanPropertyBindingResult(wachtwoordDto, "wachtwoordDto");
		wachtwoordWijzigenValidator.validate(wachtwoordDto, result);
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}

		Huisarts huisarts = getIngelogdeHuisarts();
		authenticationService.updateWachtwoord(huisarts, wachtwoordDto.getNieuweWachtwoord());

		return ResponseEntity.status(HttpStatus.OK).build();
	}

	private MedewerkerDto getMedewerkerDtoFrom(Huisarts huisarts)
	{
		var dto = new MedewerkerDto();
		dto.setRollen(huisarts.getRollen());
		dto.setId(huisarts.getHuisartsportaalId());

		var overeenkomst = overeenkomstRepository.findFirstByOrderByLaatsteWijzigDatumDesc();
		if (overeenkomst != null && huisarts.getOvereenkomstGeaccordeerdDatum() != null)
		{
			var getekend = overeenkomst.getLaatsteWijzigDatum().before(huisarts.getOvereenkomstGeaccordeerdDatum());
			dto.setOvereenkomstGetekend(getekend);
			if (!getekend)
			{
				List<Recht> overeenkomstRechten = new ArrayList<>();

				if (dto.getRollen().contains(Recht.ROLE_REGISTEREN))
				{
					overeenkomstRechten.add(Recht.ROLE_REGISTEREN);
				}

				overeenkomstRechten.add(Recht.ROLE_OVEREENKOMST);
				dto.setRollen(overeenkomstRechten);
			}
		}
		if (huisarts.getRollen().isEmpty() && huisarts.getOvereenkomstGeaccordeerdDatum() == null)
		{
			dto.setOvereenkomstGetekend(true);
		}
		return dto;
	}
}
