package nl.rivm.screenit.main.aspect;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.hibernate.Hibernate;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class EntityDeproxyProcessorAspect
{

	@Around("@within(org.springframework.stereotype.Service)")
	public Object deproxyEntiteitenVoorServices(ProceedingJoinPoint joinPoint) throws Throwable
	{
		var aangepasteArgs = Arrays.stream(joinPoint.getArgs())
			.map(this::deproxyLijstOfObject)
			.toArray();

		return joinPoint.proceed(aangepasteArgs);
	}

	private Object deproxyLijstOfObject(Object arg)
	{
		if (arg instanceof List<?> lijst)
		{
			return deproxyLijst(lijst);
		}
		if (arg instanceof Map<?, ?> map)
		{
			return deproxyMap(map);
		}
		return deproxyHibernateObject(arg);
	}

	private List<Object> deproxyLijst(List<?> lijst)
	{
		var deproxiedLijst = lijst.stream().map(this::deproxyHibernateObject);
		if (lijst.getClass().getName().startsWith("java.util.ImmutableCollections$"))
		{
			return deproxiedLijst.toList();
		}
		return deproxiedLijst.collect(Collectors.toCollection(ArrayList::new));
	}

	private LinkedHashMap<Object, Object> deproxyMap(Map<?, ?> map)
	{
		var nieuweDeproxiedMap = new LinkedHashMap<>();
		for (var entry : map.entrySet())
		{
			var key = deproxyHibernateObject(entry.getKey());
			var value = deproxyHibernateObject(entry.getValue());
			nieuweDeproxiedMap.put(key, value);
		}
		return nieuweDeproxiedMap;
	}

	private Object deproxyHibernateObject(Object obj)
	{
		return obj instanceof HibernateObject entity ? deproxy(entity) : obj;
	}

	private Object deproxy(Object obj)
	{
		return Hibernate.unproxy(ModelProxyHelper.deproxy(obj));
	}
}
