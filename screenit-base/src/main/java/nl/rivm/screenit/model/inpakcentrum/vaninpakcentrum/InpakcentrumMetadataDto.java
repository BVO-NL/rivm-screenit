package nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum;

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

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonProperty;

@Setter
@Getter
public class InpakcentrumMetadataDto
{
	@JsonProperty("_SO_ID")
	private String screeningOrganisatieId;

	@JsonProperty("_CLIENT_NAAM")
	private String clientNaam;

	@JsonProperty("_CLIENT_ADRES")
	private String clientAdres;

	@JsonProperty("_CLIENT_POSTCODE")
	private String clientPostcode;

	@JsonProperty("_CLIENT_WOONPLAATS")
	private String clientWoonplaats;

	@JsonProperty("_CLIENT_KIX")
	private String clientKix;

	@JsonProperty("_UITNODIGINGSID")
	private String uitnodigingsId;

	@JsonProperty("_VOLGNUMMER")
	private String volgNummer;

	@JsonProperty("_BVO")
	private String bvo;

	@JsonProperty("_PROJECT")
	private String project;

	@JsonProperty("_PDF")
	private String pdf;

	@JsonProperty("_TYPE")
	private String type;
}
