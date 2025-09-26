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
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Table(schema = "mamma", name = "download_onderzoeken_verzoek")
public class MammaDownloadOnderzoekenVerzoek extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.LAZY)
	@NotAudited
	private UploadDocument zipBestand;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date aangemaaktOp;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@NotAudited
	private OrganisatieMedewerker aangemaaktDoor;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BestandStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date gewijzigdOp;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verzoek", cascade = { jakarta.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	private List<MammaDownloadOnderzoek> onderzoeken = new ArrayList<>();

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date gedownloadOp;

	public UploadDocument getZipBestand()
	{
		return zipBestand;
	}

	public void setZipBestand(UploadDocument zipBestand)
	{
		this.zipBestand = zipBestand;
	}

	public Date getAangemaaktOp()
	{
		return aangemaaktOp;
	}

	public void setAangemaaktOp(Date aangemaaktOp)
	{
		this.aangemaaktOp = aangemaaktOp;
	}

	public BestandStatus getStatus()
	{
		return status;
	}

	public void setStatus(BestandStatus status)
	{
		this.status = status;
	}

	public Date getGewijzigdOp()
	{
		return gewijzigdOp;
	}

	public void setGewijzigdOp(Date gewijzigdOp)
	{
		this.gewijzigdOp = gewijzigdOp;
	}

	public OrganisatieMedewerker getAangemaaktDoor()
	{
		return aangemaaktDoor;
	}

	public void setAangemaaktDoor(OrganisatieMedewerker aangemaaktDoor)
	{
		this.aangemaaktDoor = aangemaaktDoor;
	}

	public List<MammaDownloadOnderzoek> getOnderzoeken()
	{
		return onderzoeken;
	}

	public void setOnderzoeken(List<MammaDownloadOnderzoek> onderzoeken)
	{
		this.onderzoeken = onderzoeken;
	}

	public Date getGedownloadOp()
	{
		return gedownloadOp;
	}

	public void setGedownloadOp(Date gedownloadOp)
	{
		this.gedownloadOp = gedownloadOp;
	}

}
