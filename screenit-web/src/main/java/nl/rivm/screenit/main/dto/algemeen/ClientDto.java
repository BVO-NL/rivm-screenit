package nl.rivm.screenit.main.dto.algemeen;

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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.dto.EntityDto;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;

import io.swagger.v3.oas.annotations.media.Schema;

@Getter
@Setter
@Schema(name = "ClientDto", description = "Clientgegevens voor weergave in het medewerkerportaal.")
public class ClientDto extends EntityDto
{
	private String voornaam;
	private String achternaam;
	private String tussenvoegsel;
	private String titel;
	private LocalDate geboortedatum;
	private String bsn;
	private String plaats;
	private String postcode;
	private String straat;
	private String volledigeAdres;
	private Geslacht geslacht;
	private NaamGebruik naamGebruik;
	private String partnerTussenvoegsel;
	private String partnerAchternaam;

	@Schema(description = "Heeft de client een tijdelijk adres?")
	private boolean tijdelijkAdres;
	private LocalDate overlijdensdatum;

	private String screeningsorganisatie;

	private boolean actief;
}
