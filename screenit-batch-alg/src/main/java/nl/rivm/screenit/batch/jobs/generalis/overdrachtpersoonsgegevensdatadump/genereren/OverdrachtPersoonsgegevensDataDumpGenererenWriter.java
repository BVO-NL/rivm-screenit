package nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.genereren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.jobs.generalis.overdrachtpersoonsgegevensdatadump.OverdrachtPersoonsgegevensDataDumpListener;
import nl.rivm.screenit.batch.jobs.helpers.BaseWriter;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;

import org.springframework.stereotype.Component;

@Slf4j
@AllArgsConstructor
@Component
public class OverdrachtPersoonsgegevensDataDumpGenererenWriter extends BaseWriter<OverdrachtPersoonsgegevens>
{
	private final BaseOverdrachtPersoonsgegevensService baseOverdrachtPersoonsgegevensService;

	@Override
	protected void write(OverdrachtPersoonsgegevens overdracht) throws Exception
	{
		try
		{
			baseOverdrachtPersoonsgegevensService.genereerEnPersisteerDataDump(overdracht);
			LOG.info("Data dump gegenereerd voor overdracht persoonsgegevens '{}'", overdracht.getId());
			aantalContextOphogen(OverdrachtPersoonsgegevensDataDumpListener.TOTAAL_AANTAL_GEGENEREERD_KEY);
		}
		catch (Exception ex)
		{
			LOG.error("Fout bij genereren data dump voor overdracht persoonsgegevens '{}'", overdracht.getId(), ex);
		}
	}
}
