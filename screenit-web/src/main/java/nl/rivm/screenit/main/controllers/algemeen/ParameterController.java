package nl.rivm.screenit.main.controllers.algemeen;

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

import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.dto.algemeen.ParameterDto;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@RequiredArgsConstructor
@RestController
@RequestMapping("/api/algemeen/parameters")
public class ParameterController
{

	private static final Set<PreferenceKey> TOEGESTANE_PREFERENCE_KEYS = EnumSet.of(
		PreferenceKey.COLON_START_DIGITALE_INTAKE
	);

	private final SimplePreferenceService preferenceService;

	@GetMapping
	@SecurityConstraint(actie = Actie.INZIEN, constraint = ShiroConstraint.HasPermission, recht = {}, altijdToegestaan = true, bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
	public ResponseEntity<List<ParameterDto<?>>> getParameters()
	{
		var parameters = new java.util.ArrayList<ParameterDto<?>>();
		for (var pref : TOEGESTANE_PREFERENCE_KEYS)
		{
			parameters.add(maakPreferenceDto(pref));
		}
		return ResponseEntity.ok(parameters);
	}

	private ParameterDto<?> maakPreferenceDto(PreferenceKey pref)
	{
		switch (pref.getType().getSimpleName())
		{
		case "String":
		case "LocalTime":
		case "Date":
		case "Enum":
			var dtoString = new ParameterDto<String>();
			dtoString.setWaarde(preferenceService.getString(pref.name()));
			dtoString.setNaam(pref.name());
			return dtoString;
		case "Boolean":
			var dtoBool = new ParameterDto<Boolean>();
			dtoBool.setWaarde(preferenceService.getBoolean(pref.name()));
			dtoBool.setNaam(pref.name());
			return dtoBool;
		case "Integer":
			var dtoInt = new ParameterDto<Integer>();
			dtoInt.setWaarde(preferenceService.getInteger(pref.name()));
			dtoInt.setNaam(pref.name());
			return dtoInt;
		case "Long":
			var dtoLong = new ParameterDto<Long>();
			dtoLong.setWaarde(preferenceService.getLong(pref.name()));
			dtoLong.setNaam(pref.name());
			return dtoLong;
		default:
			throw new IllegalArgumentException("Onbekend type: " + pref.getType().getSimpleName());
		}
	}
}
