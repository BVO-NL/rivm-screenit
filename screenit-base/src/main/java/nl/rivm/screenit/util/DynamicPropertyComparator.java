package nl.rivm.screenit.util;

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

import java.lang.reflect.InvocationTargetException;
import java.util.Comparator;

import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;

public class DynamicPropertyComparator<T> implements Comparator<T>
{
	private final String property;

	private final Comparator<Object> comparator;

	public DynamicPropertyComparator(String property, boolean ignoreCase, boolean ascending)
	{
		this.property = property;
		Comparator<Object> waardeComparator = ignoreCase ? DynamicPropertyComparator::vergelijkHoofdletterOngevoelig : DynamicPropertyComparator::vergelijk;
		var nullsLast = Comparator.nullsLast(waardeComparator);
		this.comparator = ascending ? nullsLast : nullsLast.reversed();
	}

	@Override
	public int compare(T object1, T object2)
	{
		return comparator.compare(getWaarde(object1), getWaarde(object2));
	}

	private Object getWaarde(T object)
	{
		try
		{
			return PropertyUtils.getNestedProperty(object, property);
		}
		catch (NestedNullException e)
		{
			return null;
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{
			throw new IllegalArgumentException("Property '%s' kan niet worden gelezen van %s".formatted(property, object.getClass().getName()), e);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static int vergelijk(Object waarde1, Object waarde2)
	{
		return ((Comparable) waarde1).compareTo(waarde2);
	}

	private static int vergelijkHoofdletterOngevoelig(Object waarde1, Object waarde2)
	{
		if (waarde1 instanceof String string1 && waarde2 instanceof String string2)
		{
			return String.CASE_INSENSITIVE_ORDER.compare(string1, string2);
		}
		return vergelijk(waarde1, waarde2);
	}
}
