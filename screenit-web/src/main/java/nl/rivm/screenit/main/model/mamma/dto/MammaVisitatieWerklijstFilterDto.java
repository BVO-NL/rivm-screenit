package nl.rivm.screenit.main.model.mamma.dto;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;

@Getter
@Setter
public class MammaVisitatieWerklijstFilterDto
{
	private List<MammaVisitatieStatus> statussen = new ArrayList<>();

	private List<Long> beoordelingsEenheidIds = new ArrayList<>();

	private List<Long> centraleEenheidIds = new ArrayList<>();

	private Date geboortedatum;

	private LocalDate vanaf;

	private String bsn;

	private String postcode;

	private Integer huisnummer;

	private MammaOnderzoekType onderzoekType;

	private List<Long> screeningseenheidIds = new ArrayList<>();

	private List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();
}
