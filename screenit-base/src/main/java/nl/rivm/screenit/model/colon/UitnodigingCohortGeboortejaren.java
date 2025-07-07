package nl.rivm.screenit.model.colon;

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
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "algemeen")
public class UitnodigingCohortGeboortejaren extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private UitnodigingCohort uitnodigingCohort;

	@Column(nullable = false)
	private Integer geboortejaren;

	public UitnodigingCohort getUitnodigingCohort()
	{
		return uitnodigingCohort;
	}

	public void setUitnodigingCohort(UitnodigingCohort uitnodigingCohort)
	{
		this.uitnodigingCohort = uitnodigingCohort;
	}

	public Integer getGeboortejaren()
	{
		return geboortejaren;
	}

	public void setGeboortejaren(Integer geboortejaren)
	{
		this.geboortejaren = geboortejaren;
	}
}
