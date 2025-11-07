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
import java.util.Date;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Table(schema = "colon")
@Audited
public class IFOBTUitslag extends AbstractHibernateObject
{
	@Enumerated(EnumType.STRING)
	private IFOBTUitslagType type;

	private String barcode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date analyseDatum;

	private BigDecimal uitslag;

	@ManyToOne
	@NotAudited
	private IFOBTBestand bestand;

	private String flag;

	private String instrumentId;

	@Enumerated(EnumType.STRING)
	private RedenNietTeBeoordelen onbeoordeelbaarReden;
}
