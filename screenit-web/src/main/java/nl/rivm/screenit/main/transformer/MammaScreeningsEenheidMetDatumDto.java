package nl.rivm.screenit.main.transformer;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;

import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.util.DateUtil;

@Getter
@AllArgsConstructor
public class MammaScreeningsEenheidMetDatumDto
{
	private MammaScreeningsEenheid screeningsEenheid;

	private LocalDate datum;

	public MammaScreeningsEenheidMetDatumDto(MammaScreeningsEenheid screeningsEenheid, Date datum)
	{
		this(screeningsEenheid, DateUtil.toLocalDate(datum));
	}
}
