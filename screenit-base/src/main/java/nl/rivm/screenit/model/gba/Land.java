
package nl.rivm.screenit.model.gba;

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
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "algemeen", indexes = @Index(columnList = "code", name = "IDX_LAND_CODE", unique = true))
public class Land extends AbstractHibernateObject implements GbaStamtabel
{

	@Serial
	private static final long serialVersionUID = 1L;

	@Column(unique = true)
	private String code;

	private String naam;

	@Temporal(TemporalType.DATE)
	private Date beginDatum;

	@Temporal(TemporalType.DATE)
	private Date eindDatum;

	@Override
	public String getCode()
	{
		return code;
	}

	public void setCode(String code)
	{
		this.code = code;
	}

	public String getNaam()
	{
		return naam;
	}

	public void setNaam(String naam)
	{
		this.naam = naam;
	}

	public Date getBeginDatum()
	{
		return beginDatum;
	}

	public void setBeginDatum(Date beginDatum)
	{
		this.beginDatum = beginDatum;
	}

	public Date getEindDatum()
	{
		return eindDatum;
	}

	public void setEindDatum(Date eindDatum)
	{
		this.eindDatum = eindDatum;
	}

	@Override
	public String toString()
	{
		StringBuilder stringbuilder = new StringBuilder();
		stringbuilder.append("ID: ");
		stringbuilder.append(this.getId());
		stringbuilder.append(", Code: ");
		stringbuilder.append(this.getCode());
		stringbuilder.append(", Naam: ");
		stringbuilder.append(this.getNaam());
		return stringbuilder.toString();
	}
}
