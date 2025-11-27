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

import java.util.Date;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingscategorie;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "colon",
	name = "uitnodiging",
	uniqueConstraints = { @UniqueConstraint(columnNames = "uitnodigingsId") },
	indexes = {
		@Index(name = "idx_colon_uitnodiging_verstuurd", columnList = "verstuurd"),
		@Index(name = "idx_colon_uitnodiging_onderzoeks_variant", columnList = "onderzoeksVariant"),
		@Index(name = "idx_colon_uitnodiging_trackid", columnList = "trackTraceId") })
@Audited
@Getter
@Setter
public class ColonUitnodiging extends InpakbareUitnodiging<ColonScreeningRonde>
{
	@Enumerated(EnumType.STRING)
	private ColonUitnodigingscategorie uitnodigingscategorie;

	@OneToOne
	private ColonFitRegistratie gekoppeldeFitRegistratie;

	@OneToOne
	private ColonFitRegistratie gekoppeldeExtraFitRegistratie;

	@Deprecated
	@OneToOne(cascade = CascadeType.ALL)
	private ScannedAntwoordFormulier antwoordFormulier;

	@ManyToOne(optional = false)
	private ColonScreeningRonde screeningRonde;

	@Enumerated(EnumType.STRING)
	@NotAudited
	@Column(nullable = false)
	private ColonOnderzoeksVariant onderzoeksVariant;

	@Temporal(TemporalType.DATE)
	private Date uitgesteldeUitslagDatum;
}
