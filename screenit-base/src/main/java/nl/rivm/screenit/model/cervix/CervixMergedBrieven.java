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
import java.util.List;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.MergedBrieven;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "cervix", name = "merged_brieven")
@Audited
public class CervixMergedBrieven extends MergedBrieven<CervixBrief>
{
	@Serial
	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "mergedBrieven", fetch = FetchType.LAZY)
	private List<CervixBrief> brieven = new ArrayList<>();

	@Override
	public List<CervixBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<CervixBrief> brieven)
	{
		this.brieven = brieven;
	}

}
