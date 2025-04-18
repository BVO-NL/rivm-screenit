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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity(name = "regio_bvo_contact_gegevens")
@Table(schema = "algemeen")
@Audited
public class RegioBvoContactGegevens extends AbstractHibernateObject
{
	@Column(length = HibernateMagicNumber.L20)
	private String telefoon;

	@Column(length = HibernateMagicNumber.L100)
	private String email;

	@Column(length = HibernateMagicNumber.L256)
	private String clientPortaalVrijeTekst;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private Adres antwoordnummerAdres;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private Adres postbusnummerAdres;
}
