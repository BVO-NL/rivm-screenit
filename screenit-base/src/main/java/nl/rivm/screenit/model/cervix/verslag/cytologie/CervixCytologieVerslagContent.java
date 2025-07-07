package nl.rivm.screenit.model.cervix.verslag.cytologie;

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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "cervix")
@Getter
@Setter
public class CervixCytologieVerslagContent
	extends VerslagContent<CervixCytologieVerslag>
{

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false, mappedBy = "verslagContent")
	@JsonIgnore
	private CervixCytologieVerslag verslag;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "68", displayName = "Verrichting", xpaths = {
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent",
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent"
	})
	private CervixCytologieVerrichting verrichting;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "246", displayName = "Cytologie uitslag BVO BMHK")
	private CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "266", displayName = "Cytologie uitslag BVO BMHK tbv huisarts")
	private CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts cytologieUitslagBvoBmhkTbvHuisarts;

}
