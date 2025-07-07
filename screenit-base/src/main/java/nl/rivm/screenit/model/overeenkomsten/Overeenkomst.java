package nl.rivm.screenit.model.overeenkomsten;

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
import java.util.List;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Setter
@Getter
@Entity
@Table(schema = "gedeeld")
public class Overeenkomst extends AbstractHibernateObject implements IActief
{
	@Enumerated(EnumType.STRING)
	private OvereenkomstType overeenkomst;

	private String naam;

	@ManyToOne
	private UploadDocument document;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatsteUpdateDocument;

	private Boolean actief = Boolean.TRUE;

	@OneToMany(mappedBy = "overeenkomst")
	private List<AbstractAfgeslotenOvereenkomst> afgeslotenOvereenkomsten;

	@Enumerated(EnumType.STRING)
	private OrganisatieType organisatieType;
}
