package nl.rivm.screenit.model.algemeen;

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

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.ScreeningRonde;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen", name = "brief")
@Audited
public class AlgemeneBrief extends ClientBrief<ScreeningRonde, Afmelding, AlgemeneBrief>
{
	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private AlgemeneMergedBrieven mergedBrieven;

	@ManyToOne(fetch = FetchType.LAZY)
	private AlgemeneBrief herdruk;

	@Override
	@Transient
	public Afmelding getAfmelding()
	{
		return null;
	}

	@Override
	@Transient
	public void setAfmelding(Afmelding afmelding)
	{
	}

	@Override
	@Transient
	public ScreeningRonde getScreeningRonde()
	{
		return null;
	}

	@Override
	@Transient
	public void setScreeningRonde(ScreeningRonde screeningRonde)
	{
	}

	@Override
	public MergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (AlgemeneMergedBrieven) mergedBrieven;
	}

	@Override
	public AlgemeneBrief getHerdruk()
	{
		return herdruk;
	}

	@Override
	public void setHerdruk(AlgemeneBrief herdruk)
	{
		this.herdruk = herdruk;
	}
}
