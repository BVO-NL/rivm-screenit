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

import java.io.Serial;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "cervix", name = "hpv_bericht")
@Audited
public class CervixHpvBericht extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

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
	private BMHKLaboratorium laboratorium;

	@Column(nullable = false)
	private String instrumentId;

	@NotAudited
	@OneToMany(mappedBy = "hpvBericht", fetch = FetchType.LAZY)
	private List<CervixHpvBeoordeling> hpvBeoordelingen = new ArrayList<CervixHpvBeoordeling>();

	public Date getOntvangen()
	{
		return ontvangen;
	}

	public void setOntvangen(Date ontvangen)
	{
		this.ontvangen = ontvangen;
	}

	public BerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(BerichtStatus status)
	{
		this.status = status;
	}

	public String getHl7Bericht()
	{
		return hl7Bericht;
	}

	public void setHl7Bericht(String hl7Bericht)
	{
		this.hl7Bericht = hl7Bericht;
	}

	public String getMessageId()
	{
		return messageId;
	}

	public void setMessageId(String messageId)
	{
		this.messageId = messageId;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public BMHKLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public String getInstrumentId()
	{
		return instrumentId;
	}

	public void setInstrumentId(String instrumentId)
	{
		this.instrumentId = instrumentId;
	}

	public List<CervixHpvBeoordeling> getHpvBeoordelingen()
	{
		return hpvBeoordelingen;
	}

	public void setHpvBeoordelingen(List<CervixHpvBeoordeling> hpvBeoordelingen)
	{
		this.hpvBeoordelingen = hpvBeoordelingen;
	}

}
