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
import java.util.List;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.MergedBrieven;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "merged_brieven", indexes = {
	@Index(name = "IDX_MERGEDBRIEVENVERSTUURD", columnList = "geprint"),
	@Index(name = "IDX_MERGEDBRIEVENCONTROLE", columnList = "controle"),
	@Index(name = "IDX_MERGEDBRIEVENVERWIJDERD", columnList = "verwijderd")})
@Audited
public class MammaMergedBrieven extends MergedBrieven<MammaBrief>
{
	@Serial
	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "mergedBrieven", fetch = FetchType.LAZY)
	private List<MammaBrief> brieven = new ArrayList<>();

	@Override
	public List<MammaBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<MammaBrief> brieven)
	{
		this.brieven = brieven;
	}
}
