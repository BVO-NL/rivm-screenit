package nl.rivm.screenit.model.colon.verslag.pa;

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

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.rivm.screenit.model.verslag.VraagElement;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class PaVerslagContent
	extends VerslagContent<PaVerslag>
{

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = false, mappedBy = "verslagContent")
	@JsonIgnore
	private PaVerslag verslag;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "68", displayName = "Verrichting", xpaths = {
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent",
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent"
	})
	private PaVerrichting verrichting;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "147", displayName = "Pathologie : medische observatie")
	private PaPathologieMedischeObservatie pathologieMedischeObservatie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "verslagContent", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "152", displayName = "Pathologie protocol: colonbiopt (per poliep)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.210']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.550']]]",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.210']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.550']]]"
	})
	private List<PaPathologieProtocolColonbioptperPoliep> pathologieProtocolColonbioptperPoliep = new ArrayList<>();

}
