package nl.rivm.screenit.model.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import nl.rivm.screenit.model.berichten.cda.CdaConstants;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;

public class VerslagProjectVersionMapping
{

	private static final VerslagProjectVersionMapping INSTANCE = new VerslagProjectVersionMapping();

	private final Map<String, Map<VerslagType, VerslagGeneratie>> mapping = new HashMap<>();

	public static VerslagProjectVersionMapping get()
	{
		return INSTANCE;
	}

	private VerslagProjectVersionMapping()
	{

		addProjectVersion("2023-01-12T11:59:05", VerslagGeneratie.V11, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2023-11-23T21:16:46", VerslagGeneratie.V12, VerslagType.PA_LAB);
		addProjectVersion("2025-03-19T15:27:22", VerslagGeneratie.V12, VerslagType.MDL);
		addProjectVersion("2024-01-19T10:30:28", VerslagGeneratie.V12, VerslagType.CERVIX_CYTOLOGIE);
		addProjectVersion("2022-09-13T14:46:06", VerslagGeneratie.V2, VerslagType.MAMMA_PA_FOLLOW_UP);
	}

	public void addProjectVersion(String projectVersion, VerslagGeneratie generatie, VerslagType... types)
	{
		var projectMapping = new EnumMap<VerslagType, VerslagGeneratie>(VerslagType.class);
		if (types.length == 0)
		{

			var teRemove = new ArrayList<String>();
			mapping.forEach((key, value) ->
			{
				if (value.containsValue(generatie))
				{
					teRemove.add(key);
				}
			});
			teRemove.forEach(mapping::remove);
		}
		else
		{
			for (var type : types)
			{
				projectMapping.put(type, generatie);
			}
			mapping.put(projectVersion.replace(CdaConstants.ROOT_OID_PROJECT_ID + ".", ""), projectMapping);
		}
	}

	public VerslagGeneratie getGeneratie(String volledigeProjectVersion, VerslagType verslagType)
	{
		return mapping.get(volledigeProjectVersion.replace(CdaConstants.ROOT_OID_PROJECT_ID + ".", "")).get(verslagType);
	}

	public String getFirstProjectVersion(VerslagGeneratie generatie, VerslagType verslagType)
	{
		for (Entry<String, Map<VerslagType, VerslagGeneratie>> e : mapping.entrySet())
		{
			if (generatie.equals(e.getValue().get(verslagType)))
			{
				return e.getKey();
			}
		}
		return "";
	}

	public List<VerslagGeneratie> getAlleSupportedGeneraties(VerslagType verslagType)
	{
		return mapping.values().stream()
			.filter(generatiePerVerslagType -> generatiePerVerslagType.containsKey(verslagType))
			.map(generatiePerVerslagType -> generatiePerVerslagType.get(verslagType))
			.distinct()
			.toList();
	}

	public VerslagGeneratie getHighestGeneratie(VerslagType verslagType)
	{
		VerslagGeneratie highestGeneratie = null;
		for (var e : mapping.entrySet())
		{
			var verslagGeneratie = e.getValue().get(verslagType);
			if (verslagGeneratie != null && (highestGeneratie == null || verslagGeneratie.ordinal() > highestGeneratie.ordinal()))
			{
				highestGeneratie = verslagGeneratie;
			}
		}
		return highestGeneratie;
	}

	public boolean verslagTypeSupportsGeneratie(VerslagType verslagType, VerslagGeneratie generatie)
	{
		return mapping.entrySet().stream().anyMatch(e -> generatie.equals(e.getValue().get(verslagType)));
	}
}
