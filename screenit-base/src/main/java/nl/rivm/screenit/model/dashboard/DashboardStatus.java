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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Level;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Getter
@Setter
@Table(schema = "gedeeld", uniqueConstraints = { @UniqueConstraint(name = "dashboardOrganisatie", columnNames = { "type", "organisatie" }) })
public class DashboardStatus extends AbstractHibernateObject
{
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private DashboardType type;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Instelling organisatie;

	@Column(length = 100)
	private String emailadressen;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private Level level;
}
