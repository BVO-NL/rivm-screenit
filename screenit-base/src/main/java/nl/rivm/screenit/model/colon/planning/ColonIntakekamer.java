package nl.rivm.screenit.model.colon.planning;

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

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Setter
@Getter
@Entity
@Table(schema = "colon", name = "intakekamer")
@Audited
public class ColonIntakekamer extends AbstractHibernateObject implements IActief
{
	@Column(nullable = false)
	private String naam;

	@Column
	private String omschrijving;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "kamer")
	private List<ColonTijdslot> tijdslots = new ArrayList<>();

	@Column(nullable = false)
	private Boolean actief;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private ColonIntakelocatie intakelocatie;
}
