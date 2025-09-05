package nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.mederwerkerinactiveren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.MedewerkerBeheerListener;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.service.BaseMedewerkerService;

import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class MedewerkerInactiverenWriter extends BaseWriter<Medewerker>
{
	private final BaseMedewerkerService medewerkerService;

	@Override
	protected void write(Medewerker medewerker) throws Exception
	{
		LOG.info("Medewerker ('{}') wordt ge&iuml;nactiveerd", medewerker.getId());
		medewerkerService.inActiveerMedewerker(medewerker);
		aantalContextOphogen(MedewerkerBeheerListener.TOTAAL_AANTAL_MEDEWERKERS_GEINACTIVEERD);
	}
}
