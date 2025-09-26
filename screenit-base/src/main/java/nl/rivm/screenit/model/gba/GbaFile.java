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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "algemeen")
@Getter
@Setter
public class GbaFile extends AbstractHibernateObject
{
	@Column(nullable = false)
	private String naam;

	@Column(nullable = false)
	private String path;

	@ManyToOne(optional = false)
	@JsonIgnore
	private GbaVerwerkingsLog gbaVerwerkingsLog;
}
