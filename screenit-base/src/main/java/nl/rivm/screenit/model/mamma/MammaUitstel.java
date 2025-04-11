package nl.rivm.screenit.model.mamma;

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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaUitstelGeannuleerdReden;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "uitstel",
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "uitnodiging") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Getter
@Setter
public class MammaUitstel extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningRonde screeningRonde;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaUitnodiging uitnodiging;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date gemaaktOp;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date geannuleerdOp;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaUitstelGeannuleerdReden geannuleerdReden;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date streefDatum;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaats standplaats;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaUitstelReden uitstelReden;
}
