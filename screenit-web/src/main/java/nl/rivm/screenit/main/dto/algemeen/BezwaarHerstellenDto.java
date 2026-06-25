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

import lombok.Getter;
import lombok.Setter;

import org.springframework.web.multipart.MultipartFile;

import io.swagger.v3.oas.annotations.media.Schema;

@Getter
@Setter
@Schema(name = "BezwaarHerstellenDto", description = "Gegevens om een BRP-bezwaar van een client te herstellen.")
public class BezwaarHerstellenDto
{
	String bsn;

	@Schema(description = "Geboortedatum van de client als tekst in kort formaat")
	String geboortedatum;

	@Schema(description = "PDF-bestand waarmee het bezwaar hersteld wordt")
	MultipartFile bestand;
}
