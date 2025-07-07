package nl.rivm.screenit.model.dashboard;

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

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.logging.LogRegel;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Setter
@Getter
@Entity
@Table(schema = "gedeeld", uniqueConstraints = { @UniqueConstraint(name = "dashboardStatusLogRegel", columnNames = { "dashboard_status", "log_regel" }) })
public class DashboardLogRegel extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private DashboardStatus dashboardStatus;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private LogRegel logRegel;
}
