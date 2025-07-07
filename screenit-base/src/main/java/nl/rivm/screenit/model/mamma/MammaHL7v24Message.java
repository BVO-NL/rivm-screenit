package nl.rivm.screenit.model.mamma;

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
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.model.mamma.enums.MammaHL7BerichtType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "mamma", name = "hl7v24_message")
public class MammaHL7v24Message extends AbstractHibernateObject
{

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date createTime;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private MammaHL7BerichtType hl7BerichtType;

	@Column(columnDefinition = "TEXT")
	private String dtoJson;

	public Date getCreateTime()
	{
		return createTime;
	}

	public void setCreateTime(Date createTime)
	{
		this.createTime = createTime;
	}

	public MammaHL7BerichtType getHl7BerichtType()
	{
		return hl7BerichtType;
	}

	public void setHl7BerichtType(MammaHL7BerichtType hl7BerichtType)
	{
		this.hl7BerichtType = hl7BerichtType;
	}

	public String getDtoJson()
	{
		return dtoJson;
	}

	public void setDtoJson(String dtoJson)
	{
		this.dtoJson = dtoJson;
	}
}
