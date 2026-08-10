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
@Schema(name = "VervangDocumentDto", description = "Gegevens om een document te vervangen.")
public class VervangDocumentDto
{
	@Schema(description = "Entiteit kan onderzoeksresultaten actie of bezwaar zijn")
	private String entiteit;

	@Schema(description = "id van de entiteit")
	private Long id;

	@Schema(description = "PDF-bestand die het upload document moet vervangen")
	private MultipartFile bestand;
}
