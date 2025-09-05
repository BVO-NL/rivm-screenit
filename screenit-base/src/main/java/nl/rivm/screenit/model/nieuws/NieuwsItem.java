package nl.rivm.screenit.model.nieuws;

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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Audited
@Getter
@Setter
public class NieuwsItem extends AbstractHibernateObject
{

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "nieuwsItem", cascade = CascadeType.ALL)
	private List<MedewerkerNieuwsItem> medewerkerNieuwsItems;

	@Column(nullable = false)
	private String titel;

	@Column(nullable = false, length = HibernateMagicNumber.L4096)
	private String tekst;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date publicerenVanaf;

	@Column
	@Temporal(TemporalType.DATE)
	private Date publicerenTot;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private OrganisatieMedewerker gemaaktDoor;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date gemaakt;

	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker gewijzigdDoor;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date gewijzigd;

}
