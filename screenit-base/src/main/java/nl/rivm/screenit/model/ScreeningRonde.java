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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;

@Entity
@Table
@Audited
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class ScreeningRonde<D extends Dossier<?, ?>, B extends ClientBrief<?, ?, ?>, AF extends Afmelding<?, ?, ?>, U extends Uitnodiging<?>>
	extends AbstractHibernateObject
{
	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@Enumerated(EnumType.STRING)
	private ScreeningRondeStatus status;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Column(length = HibernateMagicNumber.L256)
	private String afgerondReden;

	@Column(nullable = false)
	private Boolean aangemeld;

	public abstract D getDossier();

	public abstract void setDossier(D dossier);

	public List<? extends DigitaalClientBericht> getBerichten()
	{
		return new ArrayList<>();
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	public ScreeningRondeStatus getStatus()
	{
		return status;
	}

	public void setStatus(ScreeningRondeStatus status)
	{
		this.status = status;
	}

	public Date getStatusDatum()
	{
		return statusDatum;
	}

	public void setStatusDatum(Date statusDatum)
	{
		this.statusDatum = statusDatum;
	}

	public String getAfgerondReden()
	{
		return afgerondReden;
	}

	public void setAfgerondReden(String afgerondReden)
	{
		this.afgerondReden = afgerondReden;
	}

	public abstract List<U> getUitnodigingen();

	public abstract void setUitnodigingen(List<U> uitnodigingen);

	public abstract U getLaatsteUitnodiging();

	public abstract void setLaatsteUitnodiging(U laatsteUitnodiging);

	public Boolean getAangemeld()
	{
		return aangemeld;
	}

	public void setAangemeld(Boolean aangemeld)
	{
		this.aangemeld = aangemeld;
	}

	public abstract List<B> getBrieven();

	public abstract void setBrieven(List<B> brieven);

	public abstract B getLaatsteBrief();

	public abstract void setLaatsteBrief(B laatsteBrief);

	public abstract List<AF> getAfmeldingen();

	public abstract void setAfmeldingen(List<AF> afmeldingen);

	public abstract AF getLaatsteAfmelding();

	public abstract void setLaatsteAfmelding(AF laatsteAfmelding);

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		Class aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (aClass == ColonScreeningRonde.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (aClass == CervixScreeningRonde.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (aClass == MammaScreeningRonde.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}
}
