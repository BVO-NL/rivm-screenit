package nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.rolkoppelinginactiveren;

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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.medewerkerbeheer.MedewerkerBeheerListener;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.InstellingGebruikerRol;
import nl.rivm.screenit.service.AutorisatieService;

import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class RolKoppelingInactiverenWriter extends BaseWriter<InstellingGebruikerRol>
{
	private final AutorisatieService autorisatieService;

	@Override
	protected void write(InstellingGebruikerRol rolKoppeling) throws Exception
	{
		LOG.info("InstellingGebruikerRol ('{}') wordt ge&iuml;nactiveerd", rolKoppeling.getId());
		autorisatieService.inactiveerRolKoppeling(rolKoppeling);
		aantalContextOphogen(MedewerkerBeheerListener.TOTAAL_AANTAL_ROLLEN_GEINACTIVEERD);

	}
}
