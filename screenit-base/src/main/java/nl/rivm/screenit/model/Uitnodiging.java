package nl.rivm.screenit.model;

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

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Getter
@Setter
@Entity
@Table
@Audited
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class Uitnodiging<SR extends ScreeningRonde<?, ?, ?, ?>> extends AbstractHibernateObject
{
	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date creatieDatum;

	@Temporal(TemporalType.DATE)
	private Date uitnodigingsDatum;

	public Uitnodiging()
	{
	}

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);

}
