package nl.rivm.screenit.batch.jobs.cervix.brieven.client;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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

import nl.rivm.screenit.batch.jobs.brieven.AbstractBrievenGenererenListener;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.stereotype.Component;

@Component
public class CervixBriefListener extends AbstractBrievenGenererenListener
{
	@Override
	protected String getRapportageAantalBrievenKey()
	{
		return CervixBriefConstants.RAPPORTAGEKEYAANTALBRIEVEN;
	}

	@Override
	protected LogGebeurtenis getStartLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_BRIEVEN_GENERENEN_GESTART;
	}

	@Override
	protected LogGebeurtenis getEindLogGebeurtenis()
	{
		return LogGebeurtenis.CERVIX_BRIEVEN_GENERENEN_AFGEROND;
	}

	@Override
	protected Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		return Bevolkingsonderzoek.CERVIX;
	}
}
