
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

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "standplaats_opmerking")
@Audited
public class MammaStandplaatsOpmerking extends AbstractHibernateObject implements IActief
{

	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaats standplaats;

	@Column(nullable = false, length = HibernateMagicNumber.L4096)
	private String opmerking;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	@SkipFieldForDiff
	private Date creatieDatum;

	@Column(nullable = false)
	private Boolean actief;

	public MammaStandplaats getStandplaats()
	{
		return standplaats;
	}

	public void setStandplaats(MammaStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public String getOpmerking()
	{
		return opmerking;
	}

	public void setOpmerking(String opmerking)
	{
		this.opmerking = opmerking;
	}

	public Date getCreatieDatum()
	{
		return creatieDatum;
	}

	public void setCreatieDatum(Date creatieDatum)
	{
		this.creatieDatum = creatieDatum;
	}

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

}
