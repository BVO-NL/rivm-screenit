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

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.NullFlavourQuantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class MdlMedicatiemiddel
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlMedicatie medicatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_medicatie_coloscopie", values = {
		@DSValueSetValue(code = "N05CD08", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N05BA01", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N02AB02", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AX10", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AH01", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AH02", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "A03BB01", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "N01AH03", codeSystem = "2.16.840.1.113883.6.73"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "92", displayName = "Medicatie tijdens coloscopie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer/hl7:component[hl7:substanceAdministration[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.503']]]/hl7:substanceAdministration/hl7:consumable/hl7:manufacturedProduct/hl7:manufacturedLabeledDrug/hl7:code|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.502']]]/hl7:organizer/hl7:component[hl7:substanceAdministration[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.503']]]/hl7:substanceAdministration/hl7:consumable/hl7:manufacturedProduct/hl7:manufacturedLabeledDrug/hl7:code|@code"
	}, isVerplicht = true)
	private DSValue medicatieTijdensColoscopie;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "doseringMedicatieValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "doseringMedicatieUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "doseringMedicatieNf"))
	})
	@VraagElement(conceptId = "94", displayName = "Dosering medicatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer/hl7:component[hl7:substanceAdministration[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.503']]]/hl7:substanceAdministration/hl7:doseQuantity",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.502']]]/hl7:organizer/hl7:component[hl7:substanceAdministration[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.503']]]/hl7:substanceAdministration/hl7:doseQuantity"
	}, isVerplicht = true)
	private NullFlavourQuantity doseringMedicatie;

}
