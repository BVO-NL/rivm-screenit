package nl.rivm.screenit.repository.impl;

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

import org.hibernate.ScrollableResults;

public class EmptyScrollableResults implements ScrollableResults
{
	private static final Object[] EMPTY_OBJECT_ARRAY = {};

	@Override
	public Object[] get()
	{
		return EMPTY_OBJECT_ARRAY;
	}

	@Override
	public void close()
	{
	}

	@Override
	public boolean isClosed()
	{
		return true;
	}

	@Override
	public boolean next()
	{
		return false;
	}

	@Override
	public boolean previous()
	{
		return false;
	}

	@Override
	public boolean scroll(int positions)
	{
		return false;
	}

	@Override
	public boolean position(int position)
	{
		return false;
	}

	@Override
	public boolean last()
	{
		return false;
	}

	@Override
	public boolean first()
	{
		return false;
	}

	@Override
	public void beforeFirst()
	{
	}

	@Override
	public void afterLast()
	{
	}

	@Override
	public boolean isFirst()
	{
		return false;
	}

	@Override
	public boolean isLast()
	{
		return false;
	}

	@Override
	public int getRowNumber()
	{
		return -1;
	}

	@Override
	public int getPosition()
	{
		return 0;
	}

	@Override
	public boolean setRowNumber(int rowNumber)
	{
		return false;
	}

	@Override
	public void setFetchSize(int fetchSize)
	{
	}
}
