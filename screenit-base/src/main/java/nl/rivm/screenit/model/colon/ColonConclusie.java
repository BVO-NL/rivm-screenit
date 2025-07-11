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

import java.io.Serial;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.enums.ColonConclusieOnHoldReden;
import nl.rivm.screenit.model.colon.enums.ColonConclusieType;
import nl.rivm.screenit.model.colon.enums.ColonGeenOnderzoekReden;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "idx_colon_conclusie_type", columnList = "type") })
@Audited
@Getter
@Setter
public class ColonConclusie extends AbstractHibernateObject
{
	@Serial
	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ColonConclusieType type;

	@Enumerated(EnumType.STRING)
	private ColonConclusieOnHoldReden onHoldReden;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private InstellingGebruiker instellingGebruiker;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date datum;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date datumColoscopie;

	@Column(nullable = true)
	private Boolean coloscopieDatumOpVerzoekClient;

	@Column(nullable = true)
	private Integer asaScore;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private ColonGeenOnderzoekReden geenOnderzoekReden;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date noShowBericht;

	private Boolean doorverwijzingBevestigd;
}
