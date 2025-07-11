
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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "colon")
public class SKMLInterneControleSet extends AbstractHibernateObject implements Comparable<SKMLInterneControleSet>
{

	@Serial
	private static final long serialVersionUID = 1L;

	@Column(nullable = false)
	private Integer volgorde;

	private String controleTekst;

	private Integer qbaseId;

	public Integer getQbaseId()
	{
		return qbaseId;
	}

	public void setQbaseId(Integer qbaseId)
	{
		this.qbaseId = qbaseId;
	}

	public String getControleTekst()
	{
		return controleTekst;
	}

	public void setControleTekst(String controleTekst)
	{
		this.controleTekst = controleTekst;
	}

	public Integer getVolgorde()
	{
		return volgorde;
	}

	public void setVolgorde(Integer volgorde)
	{
		this.volgorde = volgorde;
	}

	@Override
	public int compareTo(SKMLInterneControleSet o)
	{
		return getVolgorde().compareTo(o.getVolgorde());
	}

}
