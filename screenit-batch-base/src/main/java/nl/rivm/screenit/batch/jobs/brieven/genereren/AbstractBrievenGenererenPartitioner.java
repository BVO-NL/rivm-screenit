package nl.rivm.screenit.batch.jobs.brieven.genereren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.helpers.BasePartitioner;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.repository.algemeen.ScreeningOrganisatieRepository;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.OrganisatieService;

import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

public abstract class AbstractBrievenGenererenPartitioner extends BasePartitioner
{

	public static final String KEY_BRIEFTYPE = "brieftype";

	public static final String KEY_SCREENINGORGANISATIEID = "screeningorganisatie.id";

	@Autowired
	private OrganisatieService organisatieService;

	@Autowired
	private ScreeningOrganisatieRepository screeningOrganisatieRepository;

	@Autowired
	private BaseBriefService briefService;

	@Override
	public Map<String, ExecutionContext> setPartition(int gridSize)
	{
		var partities = new HashMap<String, ExecutionContext>(gridSize);

		var screeningsorganisaties = new ArrayList<ScreeningOrganisatie>();
		if (briefService.isOverbruggingssituatieParagonStarted())
		{
			screeningsorganisaties.add(organisatieService.getLandelijkeScreeningsorganisatie());
		}
		else
		{
			screeningsorganisaties.addAll(screeningOrganisatieRepository.findAll());
		}

		for (var screeningsorganisatie : screeningsorganisaties)
		{
			fillingData(partities, screeningsorganisatie);
		}
		return partities;
	}

	protected abstract void fillingData(Map<String, ExecutionContext> map, ScreeningOrganisatie organisatie);
}
