package nl.rivm.screenit.model.colon;

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

import nl.rivm.screenit.model.colon.enums.ColonFitAnalyseResultaatSetStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "colon", name = "fit_analyse_resultaat_set")
@Setter
@Getter
public class ColonFitAnalyseResultaatSet extends AbstractHibernateObject
{
	@Enumerated(EnumType.STRING)
	private ColonFitAnalyseResultaatSetStatus status;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@ManyToOne
	private ColonFitLaboratorium laboratorium;

	@OneToMany(mappedBy = "analyseResultaatSet")
	private List<ColonFitAnalyseResultaat> uitslagen = new ArrayList<>();

	private String naamBestand;

	private String pathBestand;

	private Integer aantalVerwerkt = 0;

	private Integer aantalControleUitslagen = 0;
}
