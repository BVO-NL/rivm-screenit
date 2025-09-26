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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Getter
@Setter
@Entity
@Table(schema = "algemeen")
public class GbaFoutRegel extends AbstractHibernateObject
{
	@Column(length = HibernateMagicNumber.L1024, nullable = false)
	private String fout;

	@Enumerated
	@Column(nullable = false)
	private GbaFoutCategorie foutCategorie;

	private Long client;

	@ManyToOne(optional = false)
	@JsonIgnore
	private GbaVerwerkingsLog verwerkingsLog;

	@Override
	public String toString()
	{
		return "GbaFoutRegel [fout=" + fout + ", foutCategorie=" + foutCategorie + "]";
	}

}
