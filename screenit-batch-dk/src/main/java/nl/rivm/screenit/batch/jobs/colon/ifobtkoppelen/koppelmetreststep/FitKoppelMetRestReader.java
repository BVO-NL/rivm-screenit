package nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.koppelmetreststep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import nl.rivm.screenit.batch.jobs.colon.ifobtkoppelen.IfobtKoppelenConstants;
import nl.rivm.screenit.batch.jobs.helpers.BaseKoppelRestReader;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;

import org.springframework.batch.item.ItemReader;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
public class FitKoppelMetRestReader extends BaseKoppelRestReader implements ItemReader<InpakcentrumKoppelDataDto>
{
	private final LogService logService;

	@Override
	protected void logEindValidatie(LogEvent eindEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.IFOBT_CONTROLE_KOPPELEN_AFGEROND, eindEvent, Bevolkingsonderzoek.COLON);
	}

	@Override
	protected void logKoppelFout(LogEvent logEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.IFOBT_CONTROLE_KOPPELEN_FOUT, logEvent, Bevolkingsonderzoek.COLON);
	}

	@Override
	protected String getKoppelenConstant()
	{
		return IfobtKoppelenConstants.RAPPORTAGEKEYIFOBTKOPPELEN;
	}
}
