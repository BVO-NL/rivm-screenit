
package nl.rivm.screenit.model.formulieren;

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

import javax.persistence.Entity;

import nl.topicuszorg.formulieren2.persistence.resultaat.AbstractEnkelvoudigAntwoord;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "verslag.cache")
public class PalgaNumberAntwoord extends AbstractEnkelvoudigAntwoord<PalgaNumber>
{

	private static final long serialVersionUID = 1L;

	private PalgaNumber value;

	@Override
	public PalgaNumber getValue()
	{
		return value;
	}

	@Override
	public void setValue(PalgaNumber value)
	{
		this.value = value;
	}

}
