package nl.rivm.screenit.model.logging;

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
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.verwerkingverslag.cervix.CervixHuisartsberichtenRapportage;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "gedeeld")
public class CervixHuisartsberichtenBeeindigdLogEvent extends LogEvent
{
	@OneToOne(cascade = jakarta.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private CervixHuisartsberichtenRapportage rapportage;

	public CervixHuisartsberichtenRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(CervixHuisartsberichtenRapportage rapportage)
	{
		this.rapportage = rapportage;
	}
}
