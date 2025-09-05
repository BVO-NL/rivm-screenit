package nl.rivm.screenit.service.impl;

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

import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InpakcentrumService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@AllArgsConstructor
public class InpakcentrumServiceImpl implements InpakcentrumService
{
	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(readOnly = true)
	public boolean gebruikNieuweInpakcentrumKoppeling()
	{
		var startNieuweInpakcentrumKoppelingString = preferenceService.getString(PreferenceKey.START_NIEUWE_INPAKCENTRUM_KOPPELING.name(), "20260101");
		var startNieuweInpakcentrumKoppeling = DateUtil.parseLocalDateForPattern(startNieuweInpakcentrumKoppelingString, Constants.DATE_FORMAT_YYYYMMDD);
		return !currentDateSupplier.getLocalDate().isBefore(startNieuweInpakcentrumKoppeling);
	}
}
