package nl.rivm.screenit.model.verwerkingverslag.mamma;

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
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "mamma", name = "interval_uitnodigen_rapportage",
	uniqueConstraints = @UniqueConstraint(columnNames = { "uitnodigen_rapportage", "screening_organisatie" }))
@Getter
@Setter
public class MammaIntervalUitnodigenRapportage extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaUitnodigenRapportage uitnodigenRapportage;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private ScreeningOrganisatie screeningOrganisatie;

	@Column(nullable = false)
	private long uitgenodigdOpen = 0L;

	@Column(nullable = false)
	private long uitgenodigdMinderValide = 0L;

	@Column(nullable = false)
	private long uitgenodigdSuspect = 0L;
}
