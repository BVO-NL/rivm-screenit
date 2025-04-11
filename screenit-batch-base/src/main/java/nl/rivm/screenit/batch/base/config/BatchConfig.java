package nl.rivm.screenit.batch.base.config;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import javax.sql.DataSource;

import org.springframework.batch.core.Step;
import org.springframework.batch.core.repository.ExecutionContextSerializer;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.repository.dao.Jackson2ExecutionContextStringSerializer;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.SyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.lob.DefaultLobHandler;
import org.springframework.jdbc.support.lob.LobHandler;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

@Configuration
public class BatchConfig
{

	@Bean
	public TaskExecutor taskExecutor()
	{
		return new SyncTaskExecutor();
	}

	@Bean
	public TaskExecutor threadTaskExecutor()
	{
		var taskExecutor = new ThreadPoolTaskExecutor();
		taskExecutor.setCorePoolSize(8);
		taskExecutor.setMaxPoolSize(8);
		return taskExecutor;
	}

	@Bean
	public LobHandler lobHandler()
	{
		return new DefaultLobHandler();
	}

	@Bean
	public ExecutionContextSerializer executionContextSerializer()
	{
		var objectMapper = JsonMapper.builder();
		objectMapper.addModule(new JavaTimeModule());
		objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
		var serializer = new Jackson2ExecutionContextStringSerializer(
			"nl.rivm.screenit.model.logging.IntakeMakenLogEvent",
			"nl.rivm.screenit.model.colon.IntakeMakenLogEventRegel",
			"nl.rivm.screenit.batch.model.dto.MammaIlmRetryDto"
		);
		serializer.setObjectMapper(objectMapper.build());
		return serializer;
	}

	@Bean
	public JdbcTemplate jdbcTemplate(DataSource dataSource)
	{
		var jdbcTemplate = new JdbcTemplate();
		jdbcTemplate.setDataSource(dataSource);
		return jdbcTemplate;
	}

	@Bean
	public Step dummyStep(JobRepository jobRepository, HibernateTransactionManager transactionManager)
	{
		return new StepBuilder("dummyStep", jobRepository)
			.tasklet((contribution, chunkContext) -> RepeatStatus.FINISHED, transactionManager)
			.build();
	}

}
