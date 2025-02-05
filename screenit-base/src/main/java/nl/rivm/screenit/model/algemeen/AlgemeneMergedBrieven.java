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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import nl.rivm.screenit.model.MergedBrieven;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen", name = "merged_brieven")
@Audited
public class AlgemeneMergedBrieven extends MergedBrieven<AlgemeneBrief>
{
	@OneToMany(mappedBy = "mergedBrieven", cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private List<AlgemeneBrief> brieven = new ArrayList<>();

	@Override
	public List<AlgemeneBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<AlgemeneBrief> brieven)
	{
		this.brieven = brieven;
	}
}
