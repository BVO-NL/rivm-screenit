
package nl.rivm.screenit.model;

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
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class MergedBrieven<B extends Brief> extends AbstractHibernateObject implements IActief
{

	@Serial
	private static final long serialVersionUID = 1L;

	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@ManyToOne
	@NotAudited
	private UploadDocument mergedBrieven;

	@Column(nullable = false)
	private Boolean geprint = false;

	@Column(nullable = false)
	private Boolean controle = false;

	@Column(nullable = false)
	private Boolean vrijgegeven = false;

	private boolean verwijderd;

	@Temporal(TemporalType.TIMESTAMP)
	private Date printDatum;

	@ManyToOne
	private Medewerker afgedruktDoor;

	@ManyToOne
	private ScreeningOrganisatie screeningOrganisatie;

	@Column(nullable = false)
	private Integer aantalBrieven = Integer.valueOf(0);

	@Temporal(TemporalType.TIMESTAMP)
	private Date controleerDatum;

	public BriefType getBriefType()
	{
		return briefType;
	}

	public void setBriefType(BriefType briefType)
	{
		this.briefType = briefType;
	}

	public UploadDocument getMergedBrieven()
	{
		return mergedBrieven;
	}

	public void setMergedBrieven(UploadDocument mergedBrieven)
	{
		this.mergedBrieven = mergedBrieven;
	}

	public Boolean getGeprint()
	{
		return geprint;
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

	public void setGeprint(Boolean geprint)
	{
		this.geprint = geprint;
	}

	public Date getPrintDatum()
	{
		return printDatum;
	}

	public void setPrintDatum(Date printDatum)
	{
		this.printDatum = printDatum;
	}

	public boolean isVerwijderd()
	{
		return verwijderd;
	}

	public void setVerwijderd(boolean verwijderd)
	{
		this.verwijderd = verwijderd;
	}

	@Override
	@Transient
	public Boolean getActief()
	{
		return getGeprint();
	}

	@Override
	@Transient
	public void setActief(Boolean actief)
	{
		setGeprint(actief);
	}

	public Medewerker getAfgedruktDoor()
	{
		return afgedruktDoor;
	}

	public void setAfgedruktDoor(Medewerker afgedruktDoor)
	{
		this.afgedruktDoor = afgedruktDoor;
	}

	public Date getControleerDatum()
	{
		return controleerDatum;
	}

	public void setControleerDatum(Date controleerDatum)
	{
		this.controleerDatum = controleerDatum;
	}

	public Boolean getControle()
	{
		return controle;
	}

	public void setControle(Boolean controle)
	{
		this.controle = controle;
	}

	public Boolean getVrijgegeven()
	{
		return vrijgegeven;
	}

	public void setVrijgegeven(Boolean vrijgegegeven)
	{
		this.vrijgegeven = vrijgegegeven;
	}

	public abstract List<B> getBrieven();

	public abstract void setBrieven(List<B> brieven);

	public Integer getAantalBrieven()
	{
		return aantalBrieven;
	}

	public void setAantalBrieven(Integer aantalBrieven)
	{
		this.aantalBrieven = aantalBrieven;
	}
}
