package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.util.List;

import lombok.Setter;

import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import generated.KOPPELDATA;

@Setter
public abstract class BaseValiderenService
{
	@Autowired
	protected LogService logService;

	protected String getMatchingFieldValue(KOPPELDATA.VERZONDENUITNODIGING verzondenUitnodiging, String matchingFieldName)
	{
		for (var matchingField : verzondenUitnodiging.getMATCHINGFIELDS().getMATCHINGFIELD())
		{
			if (matchingField.getNAME().equalsIgnoreCase(matchingFieldName))
			{
				return matchingField.getVALUE();
			}
		}

		return null;
	}

	protected void addFout(List<String> foutmeldingen, String melding)
	{
		var logEvent = new LogEvent();
		logEvent.setMelding(melding);
		logEvent.setLevel(Level.WARNING);

		logKoppelenFout(logEvent);

		foutmeldingen.add(melding);
	}

	protected abstract void logKoppelenFout(LogEvent logEvent);

	protected boolean barcodeAlTeruggekoppeld(List<String> barcodes, String barcodeNieuw)
	{
		return barcodes.stream()
			.filter(StringUtils::isNotBlank)
			.anyMatch(barcode -> barcode.equals(barcodeNieuw));
	}
}
