package nl.rivm.screenit.model.batch;

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

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Level;

public class BvoZoekCriteria implements Serializable, IBevolkingsonderzoek
{

	@Serial
	private static final long serialVersionUID = 1L;

	private List<Bevolkingsonderzoek> bevolkingsonderzoeken = new ArrayList<>();

	private List<Level> loggingLevels;

	private Boolean exactMatch = Boolean.FALSE;

	@Override
	public List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return bevolkingsonderzoeken;
	}

	public List<Level> getLoggingLevels()
	{
		return loggingLevels;
	}

	public void setLoggingLevels(List<Level> levels)
	{
		this.loggingLevels = levels;
	}

	@Override
	public void setBevolkingsonderzoeken(List<Bevolkingsonderzoek> bevolkingsonderzoeken)
	{
		this.bevolkingsonderzoeken = bevolkingsonderzoeken;
	}

	@Override
	public Boolean getExactMatch()
	{
		return exactMatch;
	}

	@Override
	public void setExctMatch(Boolean exactMatch)
	{
		this.exactMatch = exactMatch;
	}

}
