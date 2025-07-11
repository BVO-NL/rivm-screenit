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

import java.io.Serial;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Index;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Proxy;

@Entity(name = "screenit_titel")
@Table(schema = "algemeen", indexes = @Index(name = "titel_actiefIndex", columnList = "actief"))
@Proxy(lazy = true)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class Titel extends AbstractHibernateObject implements INaam
{

	@Serial
	private static final long serialVersionUID = 1L;

	private String titel;

	@Column(nullable = false)
	private Boolean actief;

	public Boolean getActief()
	{
		return this.actief;
	}

	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	public String getTitel()
	{
		return this.titel;
	}

	public void setTitel(String titel)
	{
		this.titel = titel;
	}

	@Override
	public String getNaam()
	{
		return this.titel;
	}
}
