package nl.rivm.screenit.config;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.sql.DataSource;

import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.dao.impl.HibernateServiceImpl;
import nl.topicuszorg.hibernate.spring.util.naming.ImplicitHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.naming.PhysicalHibernate4LegacyNamingStrategy;
import nl.topicuszorg.hibernate.spring.util.sessionfactory.TopicusPostConfigurationSessionFactoryBean;

import org.hibernate.SessionFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.sql.init.dependency.DependsOnDatabaseInitialization;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.orm.hibernate5.LocalSessionFactoryBuilder;
import org.springframework.orm.jpa.support.SharedEntityManagerBean;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;

import com.zaxxer.hikari.HikariDataSource;

@Configuration
@EnableTransactionManagement
public class BaseHibernateConfig
{

	@ConfigurationProperties(prefix = "spring.datasource.hikari")
	@Bean
	@Profile("!cucumber & !test")
	HikariDataSource dataSource()
	{
		return new HikariDataSource();
	}

	@Bean(name = { "entityManagerFactory", "hibernateSessionFactory" })
	@Profile("!test & !cucumber")
	public TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory(DataSource dataSource, Optional<List<Resource>> additionalHibernateConfigLocations)
	{
		var hibernateSessionFactory = new TopicusPostConfigurationSessionFactoryBean()
		{
			@Override
			protected SessionFactory buildSessionFactory(LocalSessionFactoryBuilder sfb)
			{
				sfb.setImplicitNamingStrategy(new ImplicitHibernate4LegacyNamingStrategy());
				sfb.setPhysicalNamingStrategy(new PhysicalHibernate4LegacyNamingStrategy());
				return super.buildSessionFactory(sfb);
			}
		};

		List<Resource> configLocations = new ArrayList<>(Arrays.asList(
			new ClassPathResource("hibernate.cfg.xml"),
			new ClassPathResource("hibernate-spring-boot.cfg.xml"),
			new ClassPathResource("hibernate-dataset-mapping.cfg.xml"),
			new ClassPathResource("hibernate-mapping.cfg.xml"),
			new ClassPathResource("hibernate-organisatie.cfg.xml"),
			new ClassPathResource("hibernate-persoonsgegevens.cfg.xml")));
		additionalHibernateConfigLocations.ifPresent(configLocations::addAll);
		hibernateSessionFactory.setConfigLocations(configLocations.toArray(Resource[]::new));

		hibernateSessionFactory.setDataSource(dataSource);

		return hibernateSessionFactory;
	}

	@Bean
	@Profile("!test")
	HibernateService hibernateService()
	{
		return new HibernateServiceImpl();
	}

	@Bean
	@DependsOnDatabaseInitialization
	HibernateTransactionManager transactionManager(TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory)
	{
		var transactionManager = new HibernateTransactionManager();
		transactionManager.setSessionFactory(hibernateSessionFactory.getObject());
		transactionManager.setTransactionSynchronization(AbstractPlatformTransactionManager.SYNCHRONIZATION_ON_ACTUAL_TRANSACTION);
		return transactionManager;
	}

	@Bean
	SharedEntityManagerBean sharedEntityManager(TopicusPostConfigurationSessionFactoryBean hibernateSessionFactory)
	{
		var sharedEntityManager = new SharedEntityManagerBean();
		sharedEntityManager.setEntityManagerFactory(hibernateSessionFactory.getObject());
		return sharedEntityManager;
	}
}
