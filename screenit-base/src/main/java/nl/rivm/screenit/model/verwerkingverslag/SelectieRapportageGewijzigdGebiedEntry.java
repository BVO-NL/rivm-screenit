
package nl.rivm.screenit.model.verwerkingverslag;

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

import nl.rivm.screenit.model.colon.UitnodigingsGebied;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "colon")
public class SelectieRapportageGewijzigdGebiedEntry extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private SelectieRapportage rapportage;

	private Integer percentage;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private UitnodigingsGebied uitnodigingsGebied;

	public SelectieRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(SelectieRapportage rapportage)
	{
		this.rapportage = rapportage;
	}

	public UitnodigingsGebied getUitnodigingsGebied()
	{
		return uitnodigingsGebied;
	}

	public void setUitnodigingsGebied(UitnodigingsGebied uitnodigingsGebied)
	{
		this.uitnodigingsGebied = uitnodigingsGebied;
	}

	public Integer getPercentage()
	{
		return percentage;
	}

	public void setPercentage(Integer percentage)
	{
		this.percentage = percentage;
	}
}
