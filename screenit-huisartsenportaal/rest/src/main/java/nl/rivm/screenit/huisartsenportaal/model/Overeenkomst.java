package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import lombok.Getter;
import lombok.Setter;

import org.hibernate.envers.Audited;

import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Audited
@Getter
@Setter
public class Overeenkomst extends AbstractReferenceObject
{
	private String path;

	private String fileName;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatsteWijzigDatum;

}
