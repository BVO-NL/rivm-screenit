package nl.rivm.screenit.model.berichten.cda;

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
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity(name = "cda_melding_ongeldig_bericht")
@Table(schema = "gedeeld", indexes = @Index(name = "idx_cda_melding_ongeldig_bericht_actief", columnList = "actief"))
@Getter
@Setter
public class MeldingOngeldigCdaBericht extends AbstractHibernateObject
{

	@ManyToOne
	private OntvangenCdaBericht ontvangenCdaBericht;

	@ManyToOne
	private Instelling uitvoerendeOrganisatie;

	@ManyToOne
	private Gebruiker uitvoerder;

	@Column(length = HibernateMagicNumber.L512)
	private String melding;

	@Column(nullable = false)
	private Boolean herstelbaar;

	@Column(nullable = false)
	private Boolean actief;

	@Column(length = HibernateMagicNumber.L20)
	private String topdeskTicket;

	@Column(length = HibernateMagicNumber.L12)
	private String bsn;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datum;

}
