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

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Setter
@Getter
@Entity
@Table
@Audited
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class HuisartsBericht extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ScreeningOrganisatie screeningsOrganisatie;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private HuisartsBerichtType berichtType;

	@Column(length = 7000)
	private String berichtInhoud;

	private Date aanmaakDatum;

	@Column(nullable = false)
	private boolean isEenOpnieuwVerzondenBericht;

}
