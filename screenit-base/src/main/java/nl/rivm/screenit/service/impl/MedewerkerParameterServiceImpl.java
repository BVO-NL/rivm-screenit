package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Map;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.MedewerkerParameter;
import nl.rivm.screenit.model.MedewerkerParameterKey;
import nl.rivm.screenit.repository.algemeen.MedewerkerParameterRepository;
import nl.rivm.screenit.service.MedewerkerParameterService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class MedewerkerParameterServiceImpl implements MedewerkerParameterService
{
	private final MedewerkerParameterRepository medewerkerParameterRepository;

	@Override
	public <T> T getMedewerkerParameter(Medewerker medewerker, MedewerkerParameterKey key, T defaultWaarde)
	{
		if (medewerker == null || medewerker.getId() == null)
		{
			return defaultWaarde;
		}
		return medewerkerParameterRepository.findByMedewerkerAndKey(medewerker, key)
			.map(parameter -> parseWaarde(key, parameter.getValue(), defaultWaarde))
			.orElse(defaultWaarde);
	}

	@Override
	@Transactional
	public void saveVoorkeuren(Medewerker medewerker, Map<MedewerkerParameterKey, Boolean> voorkeuren)
	{
		for (var entry : voorkeuren.entrySet())
		{
			var key = entry.getKey();
			var waarde = entry.getValue();
			var genormaliseerdeWaarde = Boolean.toString(Boolean.TRUE.equals(waarde));
			var bestaandeParameter = medewerkerParameterRepository.findByMedewerkerAndKey(medewerker, key);
			if (bestaandeParameter.isPresent())
			{
				bestaandeParameter.get().setValue(genormaliseerdeWaarde);
			}
			else
			{
				var parameter = new MedewerkerParameter();
				parameter.setMedewerker(medewerker);
				parameter.setKey(key);
				parameter.setValue(genormaliseerdeWaarde);
				medewerkerParameterRepository.save(parameter);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private <T> T parseWaarde(MedewerkerParameterKey key, String value, T defaultWaarde)
	{
		if (value == null)
		{
			return defaultWaarde;
		}
		var valueType = key.getValueType();
		if (Boolean.class.equals(valueType))
		{
			return (T) Boolean.valueOf(value);
		}
		throw new IllegalArgumentException("Type " + valueType + " not supported");
	}
}
