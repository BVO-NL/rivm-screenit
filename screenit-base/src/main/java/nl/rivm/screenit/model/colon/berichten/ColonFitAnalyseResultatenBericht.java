package nl.rivm.screenit.model.colon.berichten;

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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.colon.ColonFitLaboratorium;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "colon", name = "fit_analyse_resultaten_bericht")
@Audited
@Getter
@Setter
public class ColonFitAnalyseResultatenBericht extends AbstractHibernateObject
{
	@Column(nullable = false, unique = true)
	private String messageId;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangen;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Column(nullable = false, columnDefinition = "TEXT")
	@NotAudited
	private String hl7Bericht;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ColonFitLaboratorium laboratorium;
}
