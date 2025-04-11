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

import java.io.FileInputStream;
import java.io.IOException;

import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingTotalenDto;
import nl.rivm.screenit.huisartsenportaal.dto.VerrichtingZoekObjectDto;
import nl.rivm.screenit.huisartsenportaal.exception.ValidatieException;
import nl.rivm.screenit.huisartsenportaal.service.VerrichtingenService;
import nl.rivm.screenit.huisartsenportaal.validator.VerrichtingenValidator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.Valid;

@RestController
@RequestMapping("verrichting")
public class VerrichtingController extends BaseController
{
	@Autowired
	private VerrichtingenService verrichtingenService;

	@Autowired
	private VerrichtingenValidator verrichtingenValidator;

	@InitBinder
	public void dataBinding(WebDataBinder binder)
	{

		binder.addValidators(verrichtingenValidator);
	}

	@PostMapping("/all")
	public ResponseEntity getHuidigeVerrichtingen(@Valid @RequestBody VerrichtingZoekObjectDto verrichtingDto, BindingResult result)
	{
		if (result.hasErrors())
		{
			throw new ValidatieException(result.getAllErrors());
		}
		VerrichtingTotalenDto verrichtingDtos = verrichtingenService.getVerrichtingen(getIngelogdeHuisarts(), verrichtingDto);
		return new ResponseEntity(verrichtingDtos, HttpStatus.OK);
	}

	@PostMapping(path = "/csv", produces = MediaType.APPLICATION_OCTET_STREAM_VALUE)
	public ResponseEntity<Resource> getHuidigeVerrichtingenCsv(@Valid @RequestBody VerrichtingZoekObjectDto verrichtingDto, BindingResult result) throws IOException
	{
		if (result.hasErrors())
		{
			return (ResponseEntity<Resource>) ResponseEntity.badRequest();
		}
		var verrichtingenCsv = verrichtingenService.getVerrichtingenCsv(getIngelogdeHuisarts(), verrichtingDto);

		return ResponseEntity.ok()
			.body(new InputStreamResource(new FileInputStream(verrichtingenCsv)));
	}
}
