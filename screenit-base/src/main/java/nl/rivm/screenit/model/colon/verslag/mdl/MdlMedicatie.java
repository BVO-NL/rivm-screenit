package nl.rivm.screenit.model.colon.verslag.mdl;

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
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class MdlMedicatie
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlColoscopieMedischeObservatie coloscopieMedischeObservatie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "medicatie", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "91", displayName = "Medicatiemiddel", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer/hl7:component[hl7:substanceAdministration[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.503']]]/hl7:substanceAdministration",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.502']]]/hl7:organizer/hl7:component[hl7:substanceAdministration[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.503']]]/hl7:substanceAdministration"
	})
	private List<MdlMedicatiemiddel> medicatiemiddel = new ArrayList<>();

	@Column
	@VraagElement(conceptId = "95", displayName = "Sedatie ja/nee", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.507']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.502']]]/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.507']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Boolean sedatieJanee;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_matevansedatie", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.90"),
		@DSValueSetValue(code = "NA", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "96", displayName = "Mate van sedatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.504']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.502']]]/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.504']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue mateVanSedatie;

}
