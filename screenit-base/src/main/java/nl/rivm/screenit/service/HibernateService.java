package nl.rivm.screenit.service;

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

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import jakarta.persistence.EntityManager;
import jakarta.persistence.NonUniqueResultException;

import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.Session;
import org.hibernate.boot.Metadata;
import org.jetbrains.annotations.NotNull;

@Deprecated
public interface HibernateService
{

	<T extends HibernateObject> int countAll(Class<T> clazz);

	void delete(Class<? extends HibernateObject> clazz, Serializable objectId);

	void delete(HibernateObject object);

	void deleteAll(Collection<? extends HibernateObject> objects);

	void executeSql(List<String> sqlStatements);

	int executeSql(String sql);

	<T extends HibernateObject> boolean existsOther(Class<T> declass, Serializable objectId,
		Map<String, Object> restrictions);

	<T extends HibernateObject> boolean existsOther(Class<T> declass, Serializable objectId,
		Map<String, Object> restrictions, boolean ignoreCase);

	<T extends HibernateObject> T get(Class<T> declass, Serializable id);

	<T extends HibernateObject> Class<T> getDeproxiedClass(T object);

	@Deprecated
	Session getHibernateSession();

	EntityManager getEntityManager();

	<T extends HibernateObject> T load(Class<T> declass, Serializable id);

	<T extends HibernateObject> List<T> loadAll(Class<T> declass);

	<T extends HibernateObject> List<T> loadAll(Class<T> clazz, String orderBy, boolean asc);

	void reload(HibernateObject hibernateObject);

	void save(HibernateObject object);

	void saveAll(HibernateObject... objects);

	<T extends HibernateObject> void saveOrUpdate(T object);

	<T extends HibernateObject> void saveOrUpdateAll(Collection<T> objects);

	<T extends HibernateObject> void saveOrUpdateAll(T... objects);

	<T extends HibernateObject> List<T> getByParameters(Class<T> clazz, Map<String, ?> parameters);

	<T extends HibernateObject> T getUniqueByParameters(Class<T> clazz, Map<String, ?> parameters) throws NonUniqueResultException;

	@NotNull Metadata getMetadata();
}
