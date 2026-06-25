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

import java.io.File;
import java.util.Arrays;
import java.util.NoSuchElementException;

import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseDvaBronService;

import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class BaseDvaBronServiceImpl implements BaseDvaBronService
{
	@Autowired
	private BaseBriefService baseBriefService;

	@Override
	public File maakPgoUitslagPdfBrief(ClientBrief<?, ?, ?> clientBrief) throws Exception
	{
		var origineelBriefType = clientBrief.getBriefType();
		var pgoBriefType = getPgoBriefType(origineelBriefType);
		try
		{
			clientBrief.setBriefType(pgoBriefType);
			return baseBriefService.maakPdfAVanBrief(clientBrief);
		}
		finally
		{
			clientBrief.setBriefType(origineelBriefType);
		}
	}

	protected static @NotNull BriefType getPgoBriefType(BriefType origineelBriefType)
	{
		return Arrays.stream(BriefType.values())
			.filter(briefType -> briefType.name().equals(origineelBriefType.name() + "_PGO"))
			.findFirst()
			.orElseThrow(() -> new NoSuchElementException("Geen PGO brieftype gevonden voor origineel brieftype: " + origineelBriefType.name()));
	}
}
