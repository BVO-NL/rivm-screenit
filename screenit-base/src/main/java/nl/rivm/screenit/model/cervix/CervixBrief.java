package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "brief", indexes = { @Index(name = "idx_cervix_brief_gegenereerd", columnList = "gegenereerd"),
	@Index(name = "idx_cervix_brief_vervangendeprojectbrief", columnList = "vervangendeprojectbrief") })
@Audited
@Setter
@Getter
public class CervixBrief extends ClientBrief<CervixScreeningRonde, CervixAfmelding, CervixBrief>
{

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixScreeningRonde screeningRonde;

	@OneToOne(mappedBy = "brief", fetch = FetchType.LAZY)
	private CervixUitnodiging uitnodiging;

	@OneToOne(mappedBy = "brief", fetch = FetchType.LAZY)
	private CervixMonster monster;

	@OneToOne(mappedBy = "huisartsOnbekendBrief", fetch = FetchType.LAZY)
	private CervixLabformulier labformulier;

	@ManyToOne(fetch = FetchType.LAZY)
	private CervixAfmelding afmelding;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private CervixBrief herdruk;

	@ManyToOne(fetch = FetchType.LAZY)
	private CervixMergedBrieven mergedBrieven;

	@Enumerated(EnumType.STRING)
	private PreferenceKey heraanmeldenTekstKey;

	@Enumerated(EnumType.STRING)
	private CervixOmissieType omissieType;

	@Transient
	private transient boolean aangevraagdeHerdruk = false;

	@Override
	public CervixMergedBrieven getMergedBrieven()
	{
		return mergedBrieven;
	}

	@Override
	public void setMergedBrieven(MergedBrieven mergedBrieven)
	{
		this.mergedBrieven = (CervixMergedBrieven) mergedBrieven;
	}
}
