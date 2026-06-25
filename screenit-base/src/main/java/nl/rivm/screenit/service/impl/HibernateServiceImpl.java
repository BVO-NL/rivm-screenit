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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.service.HibernateService;
import nl.rivm.screenit.config.hibernate.BaseHibernateConfig;
import nl.rivm.screenit.config.hibernate.HibernateOrmMappingResourceProvider;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;
import nl.topicuszorg.hibernate.object.model.HibernateObject;

import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.NonUniqueResultException;
import org.hibernate.Session;
import org.hibernate.boot.Metadata;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.jpa.HibernateHints;
import org.hibernate.proxy.HibernateProxy;
import org.hibernate.proxy.LazyInitializer;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.JpaEntityInformationSupport;
import org.springframework.transaction.annotation.Transactional;

@Slf4j
public class HibernateServiceImpl implements HibernateService
{

	@PersistenceContext
	private EntityManager entityManager;

	@Setter
	private HibernateOrmMappingResourceProvider ormMappingResourceProvider;

	@Override
	@Transactional
	public <T extends HibernateObject> int countAll(Class<T> clazz)
	{
		var em = getEntityManager();
		var cb = em.getCriteriaBuilder();
		var criteria = cb.createQuery(Long.class);

		var root = criteria.from(clazz);
		criteria.select(cb.count(root));

		return em.createQuery(criteria).getSingleResult().intValue();
	}

	@Override
	@Transactional
	public void delete(Class<? extends HibernateObject> clazz, Serializable objectId)
	{
		delete(load(clazz, objectId));
	}

	@Override
	@Transactional
	public void delete(HibernateObject entity)
	{
		getEntityManager().remove(entity);
	}

	@Override
	@Transactional
	public void deleteAll(Collection<? extends HibernateObject> entities)
	{
		entities.forEach(this::delete);
	}

	@Override
	@Transactional
	public void executeSql(List<String> sqlStatements)
	{
		if (sqlStatements != null)
		{
			sqlStatements.forEach(this::executeSql);
		}
	}

	@Override
	@Transactional
	public int executeSql(String sql)
	{
		return getEntityManager().createNativeQuery(sql).executeUpdate();
	}

	@Override
	public <T extends HibernateObject> boolean existsOther(Class<T> declass, Serializable objectId, Map<String, Object> restrictions)
	{
		return existsOther(declass, objectId, restrictions, false);
	}

	@Override
	public <T extends HibernateObject> boolean existsOther(Class<T> declass, Serializable objectId, Map<String, Object> restrictions,
		boolean ignoreCase)
	{
		var em = getEntityManager();
		var cb = em.getCriteriaBuilder();
		var q = cb.createQuery(Long.class);
		var r = q.from(declass);
		var predicates = new ArrayList<Predicate>();
		if (objectId != null)
		{
			predicates.add(cb.notEqual(r.get(AbstractHibernateObject_.ID), objectId));
		}
		voegRestrictieToeAanPredicates(restrictions, ignoreCase, r, cb, predicates);
		q.where(cb.and(predicates.toArray(new Predicate[predicates.size()])));
		q.select(cb.count(r.get(AbstractHibernateObject_.ID)));
		return em.createQuery(q).getSingleResult() > 0;
	}

	private static <T extends HibernateObject> void voegRestrictieToeAanPredicates(Map<String, Object> restrictions, boolean ignoreCase, Root<T> r, CriteriaBuilder cb,
		ArrayList<Predicate> predicates)
	{
		restrictions.forEach((key, value) ->
		{
			var field = r.get(key);
			Predicate predicate;
			if (value == null)
			{
				predicate = cb.isNull(field);
			}
			else if (ignoreCase && value instanceof String stringValue)
			{
				predicate = cb.like(cb.lower(field.as(String.class)), stringValue.toLowerCase());
			}
			else
			{
				predicate = cb.equal(field, value);
			}
			predicates.add(predicate);
		});
	}

	@Override
	public <T extends HibernateObject> T get(Class<T> declass, Serializable id)
	{
		return getEntityManager().find(declass, id);
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T extends HibernateObject> Class<T> getDeproxiedClass(T object)
	{
		var unproxied = (Class<T>) object.getClass();

		try
		{
			var getHibernateLazyInitializer = object.getClass().getMethod("getHibernateLazyInitializer");
			var lazyInitializer = (LazyInitializer) getHibernateLazyInitializer.invoke(object, new Object[] {});

			unproxied = (Class<T>) lazyInitializer.getPersistentClass();
		}
		catch (Exception e1)
		{
			LOG.trace(e1.getMessage());
		}

		return unproxied;
	}

	@Override
	public Session getHibernateSession()
	{
		return getEntityManager().unwrap(Session.class);
	}

	@Override
	public EntityManager getEntityManager()
	{
		return entityManager;
	}

	@Override
	public <T extends HibernateObject> T load(Class<T> declass, Serializable id)
	{
		return getEntityManager().getReference(declass, id);
	}

	@Override
	public <T extends HibernateObject> List<T> loadAll(Class<T> declass)
	{
		var em = getEntityManager();
		var cb = em.getCriteriaBuilder();
		var q = cb.createQuery(declass);
		q.from(declass);
		return em.createQuery(q).getResultList();
	}

	@Override
	public <T extends HibernateObject> List<T> loadAll(Class<T> clazz, String orderBy, boolean asc)
	{
		var q = createLoadAllCriteria(clazz, orderBy, asc);
		var query = getEntityManager().createQuery(q);
		query.setHint(HibernateHints.HINT_CACHEABLE, true);
		return query.getResultList();
	}

	private <T extends HibernateObject> CriteriaQuery<T> createLoadAllCriteria(Class<T> clazz, String orderBy, boolean asc)
	{
		var em = getEntityManager();
		var cb = em.getCriteriaBuilder();
		var q = cb.createQuery(clazz);
		var r = q.from(clazz);

		var orderPath = r.get(orderBy);
		q.orderBy(asc ? cb.asc(orderPath) : cb.desc(orderPath));
		return q;
	}

	@Override
	@Transactional
	public void reload(HibernateObject hibernateObject)
	{
		try
		{
			getEntityManager().refresh(hibernateObject);
		}
		catch (final HibernateException e)
		{
			LOG.error(e.getMessage(), e);
			throw new HibernateException(e);
		}
	}

	@Override
	@Transactional
	public void save(HibernateObject object)
	{
		getEntityManager().persist(object);
	}

	@Override
	@Transactional
	public void saveAll(HibernateObject... objects)
	{
		List.of(objects).forEach(this::save);
	}

	@Override
	@Transactional
	public <T extends HibernateObject> void saveOrUpdate(T entity)
	{
		var em = getEntityManager();
		JpaEntityInformation<T, ?> entityInformation = JpaEntityInformationSupport.getEntityInformation(Hibernate.getClassLazy(entity), em);
		if (entityInformation.isNew(entity))
		{
			em.persist(entity);
		}
		if (entity instanceof HibernateProxy proxy)
		{

			var hibernateLazyInitializer = proxy.getHibernateLazyInitializer();
			if (hibernateLazyInitializer.getInternalIdentifier() == null)
			{
				var savedId = entity.getId();
				hibernateLazyInitializer.setIdentifier(savedId);
			}
		}
	}

	@Override
	@Transactional
	public <T extends HibernateObject> void saveOrUpdateAll(Collection<T> entities)
	{
		entities.forEach(this::saveOrUpdate);
	}

	@Override
	@Transactional
	public <T extends HibernateObject> void saveOrUpdateAll(T... objects)
	{
		saveOrUpdateAll(List.of(objects));
	}

	private <T extends HibernateObject> CriteriaQuery<T> getCriteriaQueryForParameters(final Class<T> clazz, final Map<String, ?> parameters)
	{
		var cb = getEntityManager().getCriteriaBuilder();
		var q = cb.createQuery(clazz);
		var r = q.from(clazz);

		var predicates = new ArrayList<Predicate>();
		parameters.forEach((key, value) ->
		{
			final Path<Object> field = r.get(key);
			predicates.add(cb.equal(field, value));
		});
		q.where(cb.and(predicates.toArray(new Predicate[predicates.size()])));
		return q;
	}

	@Override
	public <T extends HibernateObject> List<T> getByParameters(Class<T> clazz, Map<String, ?> parameters)
	{
		if (parameters == null)
		{
			return Collections.emptyList();
		}

		final CriteriaQuery<T> criteria = getCriteriaQueryForParameters(clazz, parameters);
		return getEntityManager().createQuery(criteria).getResultList();
	}

	@Override
	public <T extends HibernateObject> T getUniqueByParameters(Class<T> clazz, Map<String, ?> parameters)
		throws NonUniqueResultException
	{
		if (parameters == null)
		{
			return null;
		}

		final CriteriaQuery<T> criteria = getCriteriaQueryForParameters(clazz, parameters);
		return ((org.hibernate.query.Query<T>) getEntityManager().createQuery(criteria)).uniqueResult();
	}

	@Override
	public @NotNull Metadata getMetadata()
	{
		var sfi = getEntityManager().getEntityManagerFactory().unwrap(SessionFactoryImplementor.class);
		var props = sfi.getProperties();

		var registry = new StandardServiceRegistryBuilder()
			.applySettings(props)
			.build();

		var sources = new MetadataSources(registry);
		sfi.getJpaMetamodel().getEntities().forEach(entity ->
			sources.addAnnotatedClass(entity.getJavaType()));
		for (var ormMappingResource : BaseHibernateConfig.maakOrmMappingResources(ormMappingResourceProvider))
		{
			sources.addResource(ormMappingResource);
		}

		var metadata = sources.buildMetadata();

		if (metadata == null)
		{
			LOG.error("Hibernate metadata-integrator is niet geregistreerd! Deze moet geregistreerd worden door de localSessionFactoryBean, zie hibernate-extension-spring.");
			throw new NullPointerException();
		}
		return metadata;
	}
}
