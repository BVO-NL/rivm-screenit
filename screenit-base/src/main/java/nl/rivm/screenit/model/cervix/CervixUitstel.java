package nl.rivm.screenit.model.cervix;

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
import jakarta.persistence.Index;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "uitstel", indexes = { @Index(name = "idx_cervix_uitstel_geannuleerd_datum", columnList = "geannuleerdDatum"),
	@Index(name = "idx_cervix_uitstel_uitstellen_tot_datum", columnList = "uitstellenTotDatum") })
@Audited
public class CervixUitstel extends AbstractHibernateObject
{
	@OneToOne(mappedBy = "uitstel", fetch = FetchType.LAZY)
	private CervixScreeningRonde screeningRonde;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private CervixUitstelType uitstelType;

	@Column(nullable = false)
	private Date uitstellenTotDatum;

	@Column(nullable = false)
	private Date wijzigingsDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date geannuleerdDatum;

	public CervixUitstelType getUitstelType()
	{
		return uitstelType;
	}

	public void setUitstelType(CervixUitstelType uitstelType)
	{
		this.uitstelType = uitstelType;
	}

	public Date getUitstellenTotDatum()
	{
		return uitstellenTotDatum;
	}

	public void setUitstellenTotDatum(Date uitstellenTotDatum)
	{
		this.uitstellenTotDatum = uitstellenTotDatum;
	}

	public Date getWijzigingsDatum()
	{
		return wijzigingsDatum;
	}

	public void setWijzigingsDatum(Date wijzigingsDatum)
	{
		this.wijzigingsDatum = wijzigingsDatum;
	}

	public CervixScreeningRonde getScreeningRonde()
	{
		return screeningRonde;
	}

	public void setScreeningRonde(CervixScreeningRonde ronde)
	{
		this.screeningRonde = ronde;
	}

	public Date getGeannuleerdDatum()
	{
		return geannuleerdDatum;
	}

	public void setGeannuleerdDatum(Date geannuleerdDatum)
	{
		this.geannuleerdDatum = geannuleerdDatum;
	}
}
