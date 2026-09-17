package nl.rivm.screenit.config;

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

import java.util.concurrent.TimeUnit;

import lombok.Getter;
import lombok.Setter;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "commhub")
public class CommunicationHubProperties
{
	private String tenant;

	private String baseUrl;

	private String idpScope;

	private String idpAudience;

	private String idpClientId;

	private String idpKeystoreLocation;

	private String idpKeystorePassword;

	private String smsSender;

	private BriefafdrukopdrachtMonitoringProperties briefafdrukopdrachtMonitoring = new BriefafdrukopdrachtMonitoringProperties();

	private BriefafdrukopdrachtSendBevestigingProperties briefafdrukopdrachtSendBevestiging = new BriefafdrukopdrachtSendBevestigingProperties();

	@Setter
	public static class BriefafdrukopdrachtMonitoringProperties
	{
		private static final int DEFAULT_QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD = 2500;

		private static final int DEFAULT_QUEUE_TE_VERSTUREN_SIZE_THRESHOLD = 2000;

		private Integer queueNotYetDequeuedSizeThreshold;

		private Integer queueTeVersturenSizeThreshold;

		public long getQueueNotYetDequeuedSizeThreshold()
		{
			return positiefOfDefault(queueNotYetDequeuedSizeThreshold, DEFAULT_QUEUE_NOT_YET_DEQUEUED_SIZE_THRESHOLD);
		}

		public long getQueueTeVersturenSizeThreshold()
		{
			return positiefOfDefault(queueTeVersturenSizeThreshold, DEFAULT_QUEUE_TE_VERSTUREN_SIZE_THRESHOLD);
		}
	}

	@Setter
	public static class BriefafdrukopdrachtSendBevestigingProperties
	{
		private static final long DEFAULT_INIT_POLL_INTERVAL_MS = TimeUnit.SECONDS.toMillis(10);

		private static final long DEFAULT_MAX_POLL_INTERVAL_MS = TimeUnit.MINUTES.toMillis(5);

		private static final int DEFAULT_MAX_POLLS_PER_RUN = 100;

		private static final long DEFAULT_MAX_RUN_DURATION_MS = TimeUnit.SECONDS.toMillis(5);

		private static final int DEFAULT_OUTAGE_THRESHOLD = 5;

		private static final int DEFAULT_OUTAGE_MULTIPLIER = 6;

		private static final long DEFAULT_LANG_GEEN_PARAGON_WAARSCHUWING_MS = TimeUnit.HOURS.toMillis(1);

		private Long initPollIntervalMs;

		private Long maxPollIntervalMs;

		private Integer maxPollsPerRun;

		private Long maxRunDurationMs;

		private Integer outageThreshold;

		private Integer outageMultiplier;

		private Long langdurigGeenParagonWaarschuwingMs;

		public long getInitPollIntervalMs()
		{
			return positiefOfDefault(initPollIntervalMs, DEFAULT_INIT_POLL_INTERVAL_MS);
		}

		public long getMaxPollIntervalMs()
		{
			return positiefOfDefault(maxPollIntervalMs, DEFAULT_MAX_POLL_INTERVAL_MS);
		}

		public int getMaxPollsPerRun()
		{
			return positiefOfDefault(maxPollsPerRun, DEFAULT_MAX_POLLS_PER_RUN);
		}

		public long getMaxRunDurationMs()
		{
			return positiefOfDefault(maxRunDurationMs, DEFAULT_MAX_RUN_DURATION_MS);
		}

		public int getOutageThreshold()
		{
			return positiefOfDefault(outageThreshold, DEFAULT_OUTAGE_THRESHOLD);
		}

		public int getOutageMultiplier()
		{
			return positiefOfDefault(outageMultiplier, DEFAULT_OUTAGE_MULTIPLIER);
		}

		public long getLangdurigGeenParagonWaarschuwingMs()
		{
			return positiefOfDefault(langdurigGeenParagonWaarschuwingMs, DEFAULT_LANG_GEEN_PARAGON_WAARSCHUWING_MS);
		}

	}

	private static <T extends Number> T positiefOfDefault(T waarde, T defaultWaarde)
	{
		return waarde != null && waarde.longValue() > 0 ? waarde : defaultWaarde;
	}
}
