package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.model.ClientAfspraak;
import nl.rivm.screenit.batch.model.IntakeSolution;
import nl.rivm.screenit.batch.service.PlanIntakeAfsprakenService;
import nl.rivm.screenit.model.colon.dto.VrijSlot;

import org.springframework.stereotype.Service;

import ai.timefold.solver.core.api.solver.SolverFactory;
import ai.timefold.solver.core.config.solver.SolverConfig;

@Service
@Slf4j
public class PlanIntakeAfsprakenServiceImpl implements PlanIntakeAfsprakenService
{
	@Override
	public List<ClientAfspraak> planIntakeAfspraken(List<ClientAfspraak> clienten, List<VrijSlot> vrijeSloten, StringBuilder planningResultaat, Long maximumSecondsSpend)
	{
		System.setProperty("javax.xml.parsers.DocumentBuilderFactory", "com.sun.org.apache.xerces.internal.jaxp.DocumentBuilderFactoryImpl");
		var solverConfig = SolverConfig.createFromXmlResource("screenit-planning-solver-config.xml");
		solverConfig.getTerminationConfig().setSecondsSpentLimit(maximumSecondsSpend);
		SolverFactory<IntakeSolution> solverFactory = SolverFactory.create(solverConfig);
		var solver = solverFactory.buildSolver();

		var intakeSolution = new IntakeSolution();

		intakeSolution.setClientAfspraken(clienten);
		intakeSolution.setVrijeSloten(vrijeSloten);
		var bestSolution = solver.solve(intakeSolution);

		LOG.trace(bestSolution.toString());
		planningResultaat.append("planner score ").append(bestSolution.getScore());
		List<ClientAfspraak> clientAfspraken = new ArrayList<>();
		if (bestSolution.getScore().hardScore() == 0)
		{
			clientAfspraken = bestSolution.getClientAfspraken();
		}
		return clientAfspraken;
	}
}
