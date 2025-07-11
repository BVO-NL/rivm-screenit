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

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "mamma", name = "standplaats_ronde_uitnodigen_rapportage")
@Getter
@Setter
public class MammaStandplaatsRondeUitnodigenRapportage extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaUitnodigenRapportage uitnodigenRapportage;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsRonde standplaatsRonde;

	@OneToMany(mappedBy = "standplaatsRondeUitnodigenRapportage", fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	private List<MammaStandplaatsPeriodeUitnodigenRapportage> standplaatsPeriodeUitnodigenRapportages = new ArrayList<>();

	@Column
	private Long totaalTotaal;

	@Column
	private Long totaalVervolgRonde;

	@Column
	private Long totaalEersteRonde;

	@Column
	private Long totaalDubbeleTijd;

	@Column
	private Long totaalMinderValide;

	@Column
	private Long totaalTehuis;

	@Column
	private Long totaalSuspect;

	@Column
	private Long uitTeNodigenTotaal;

	@Column
	private Long uitTeNodigenVervolgRonde;

	@Column
	private Long uitTeNodigenEersteRonde;

	@Column
	private Long uitTeNodigenDubbeleTijd;

	@Column
	private Long uitTeNodigenMinderValide;

	@Column
	private Long uitTeNodigenTehuis;

	@Column
	private Long uitTeNodigenSuspect;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaStandplaatsRondeRapportageStatus status;
}
