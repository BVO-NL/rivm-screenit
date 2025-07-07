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

import java.math.BigDecimal;
import java.time.LocalDateTime;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.enums.ColonAfspraakStatus;
import nl.rivm.screenit.model.colon.enums.ColonTijdslotType;
import nl.rivm.screenit.model.colon.planning.ColonAfspraakslot;
import nl.rivm.screenit.model.colon.planning.ColonTijdslot;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Getter
@Setter
@Entity
@Table(schema = "colon", name = "intakeafspraak",
	indexes = { @Index(name = "idx_colon_afspraak_bezwaar", columnList = "bezwaar") }

)
@Audited
public class ColonIntakeAfspraak extends ColonTijdslot
{

	public ColonIntakeAfspraak()
	{
		setType(ColonTijdslotType.INTAKEAFSPRAAK);
	}

	@ManyToOne
	@JoinColumn(name = "id", insertable = false, updatable = false)
	private ColonTijdslot parent;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE }, optional = false)
	@Cascade({ CascadeType.SAVE_UPDATE })
	private ColonScreeningRonde colonScreeningRonde;

	@OneToOne(fetch = FetchType.LAZY)
	private ColonConclusie conclusie;

	private LocalDateTime afgezegdOp;

	@Column(nullable = false)
	private LocalDateTime aangemaaktOp;

	@Column(nullable = false)
	private LocalDateTime gewijzigdOp;

	@Column(precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3, nullable = false)
	private BigDecimal afstand;

	@Column(nullable = false)
	private boolean bezwaar;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private ColonAfspraakStatus status;

	@OneToOne(fetch = FetchType.LAZY)
	private ColonIntakeAfspraak nieuweAfspraak;

	@ManyToOne(fetch = FetchType.LAZY)
	private ColonIntakeAfspraak oudeAfspraak;

	@ManyToOne(cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE }, optional = false)
	@Cascade(CascadeType.SAVE_UPDATE)
	private Client client;

	@OneToOne(fetch = FetchType.LAZY)
	private ColonAfspraakslot afspraakslot;
}
