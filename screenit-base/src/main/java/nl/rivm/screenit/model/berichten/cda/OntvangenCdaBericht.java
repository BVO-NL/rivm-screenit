package nl.rivm.screenit.model.berichten.cda;

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
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity(name = "cda_ontvangenbericht")
@Table(schema = "gedeeld", indexes = { @Index(name = "idx_ontvangenbericht_status", columnList = "status") })
public class OntvangenCdaBericht extends AbstractHibernateObject
{
	@Serial
	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangen;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtStatus status;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtType berichtType;

	@Column
	private String berichtId;

	@Column
	private String setId;

	@Column
	private Long versie;

	@Column(columnDefinition = "TEXT")
	private String xmlBericht;

	@Column
	private String projectVersion;

	public Date getOntvangen()
	{
		return this.ontvangen;
	}

	public void setOntvangen(Date ontvangen)
	{
		this.ontvangen = ontvangen;
	}

	public BerichtType getBerichtType()
	{
		return berichtType;
	}

	public void setBerichtType(BerichtType berichtType)
	{
		this.berichtType = berichtType;
	}

	public String getXmlBericht()
	{
		return xmlBericht;
	}

	public void setXmlBericht(String xmlBericht)
	{
		this.xmlBericht = xmlBericht;
	}

	public void setBerichtId(String berichtId)
	{
		this.berichtId = berichtId;
	}

	public String getBerichtId()
	{
		return berichtId;
	}

	public String getSetId()
	{
		return setId;
	}

	public void setSetId(String setId)
	{
		this.setId = setId;
	}

	public Long getVersie()
	{
		return versie;
	}

	public void setVersie(Long versie)
	{
		this.versie = versie;
	}

	public BerichtStatus getStatus()
	{
		return status;
	}

	public void setStatus(BerichtStatus status)
	{
		this.status = status;
	}

	public String getProjectVersion()
	{
		return projectVersion;
	}

	public void setProjectVersion(String projectVersion)
	{
		this.projectVersion = projectVersion;
	}
}
