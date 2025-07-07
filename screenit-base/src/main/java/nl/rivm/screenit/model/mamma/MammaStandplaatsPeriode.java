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
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "standplaats_periode")
@Audited
public class MammaStandplaatsPeriode extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaStandplaatsRonde standplaatsRonde;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaScreeningsEenheid screeningsEenheid;

	@Column(nullable = false)
	private Integer screeningsEenheidVolgNr;

	@Column(nullable = false)
	private Integer standplaatsRondeVolgNr;

	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date vanaf;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date totEnMet;

	@Column(nullable = false)
	private Boolean prognose = true;

	public MammaStandplaatsRonde getStandplaatsRonde()
	{
		return standplaatsRonde;
	}

	public void setStandplaatsRonde(MammaStandplaatsRonde standplaatsRonde)
	{
		this.standplaatsRonde = standplaatsRonde;
	}

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public void setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
	}

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTotEnMet()
	{
		return totEnMet;
	}

	public void setTotEnMet(Date totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	public Integer getScreeningsEenheidVolgNr()
	{
		return screeningsEenheidVolgNr;
	}

	public void setScreeningsEenheidVolgNr(Integer screeningsEenheidVolgNr)
	{
		this.screeningsEenheidVolgNr = screeningsEenheidVolgNr;
	}

	public Integer getStandplaatsRondeVolgNr()
	{
		return standplaatsRondeVolgNr;
	}

	public void setStandplaatsRondeVolgNr(Integer standplaatsRondeVolgNr)
	{
		this.standplaatsRondeVolgNr = standplaatsRondeVolgNr;
	}

	public Boolean getPrognose()
	{
		return prognose;
	}

	public void setPrognose(Boolean prognose)
	{
		this.prognose = prognose;
	}
}
