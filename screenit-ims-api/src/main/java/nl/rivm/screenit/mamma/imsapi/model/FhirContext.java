package nl.rivm.screenit.mamma.imsapi.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-ims-api
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

import java.util.List;

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

@Setter
@Getter
public class FhirContext
{
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.WRAPPER_OBJECT)
	private List<FhirWorklistItem> worklist;

	private String type;

	private String value;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private String launchUrl;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private FhirLayoutImages layoutImages;

	@JsonInclude(JsonInclude.Include.NON_NULL)
	private FhirSyncStatus syncStatus;

}
