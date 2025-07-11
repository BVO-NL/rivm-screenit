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

@Entity
@Table(schema = "algemeen", indexes = @Index(name = "functie_actiefIndex", columnList = "actief"))
@Proxy(lazy = true)
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class Functie extends AbstractHibernateObject implements INaam
{

	public static final String PATHOLOOG = "Patholoog";

	public static final String ENDOSCOPIST = "Endoscopist";

	@Serial
	private static final long serialVersionUID = 1L;

	private String functie;

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

	public String getFunctie()
	{
		return this.functie;
	}

	public void setFunctie(String functie)
	{
		this.functie = functie;
	}

	@Override
	public String getNaam()
	{
		return this.functie;
	}
}
