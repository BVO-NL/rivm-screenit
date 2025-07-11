package nl.rivm.screenit.model.verwerkingverslag.cervix;

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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "cervix", name = "herinneren_rapportage_brief_type")
public class CervixHerinnerenRapportageBriefType extends AbstractHibernateObject
{
	@ManyToOne(optional = false)
	private CervixHerinnerenRapportage rapportage;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private BriefType briefType;

	private long aantal;

	public CervixHerinnerenRapportageBriefType()
	{
	}

	public CervixHerinnerenRapportageBriefType(CervixHerinnerenRapportage rapportage, BriefType briefType, long aantal)
	{
		this.rapportage = rapportage;
		this.briefType = briefType;
		this.aantal = aantal;
	}

	public BriefType getBriefType()
	{
		return briefType;
	}

	public void setBriefType(BriefType briefType)
	{
		this.briefType = briefType;
	}

	public CervixHerinnerenRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(CervixHerinnerenRapportage rapportage)
	{
		this.rapportage = rapportage;
	}

	public long getAantal()
	{
		return aantal;
	}

	public void setAantal(long aantal)
	{
		this.aantal = aantal;
	}
}
