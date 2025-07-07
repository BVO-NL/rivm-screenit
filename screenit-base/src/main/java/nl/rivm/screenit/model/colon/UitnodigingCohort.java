
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

import java.io.Serial;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Table(schema = "algemeen")
public class UitnodigingCohort extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@Column(unique = true)
	private Integer jaar;

	@OneToMany(mappedBy = "uitnodigingCohort", fetch = FetchType.LAZY)
	private List<UitnodigingCohortGeboortejaren> geboortejaren = new ArrayList<>();

	public Integer getJaar()
	{
		return jaar;
	}

	public void setJaar(Integer jaar)
	{
		this.jaar = jaar;
	}

	public List<UitnodigingCohortGeboortejaren> getGeboortejaren()
	{
		return geboortejaren;
	}

	public void setGeboortejaren(List<UitnodigingCohortGeboortejaren> geboortejaren)
	{
		this.geboortejaren = geboortejaren;
	}
}
