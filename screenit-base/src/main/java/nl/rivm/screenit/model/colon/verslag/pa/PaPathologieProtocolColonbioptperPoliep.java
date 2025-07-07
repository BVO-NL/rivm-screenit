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

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
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
public class PaPathologieProtocolColonbioptperPoliep
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private PaVerslagContent verslagContent;

	@Column(length = 255)
	@VraagElement(conceptId = "156", displayName = "Nummer potje materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.210']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.550']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:asSpecimenInContainer/lab:container/lab:id",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.210']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.550']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:asSpecimenInContainer/lab:container/lab:id|@extension"
	}, isVerplicht = true)
	private String nummerPotjeMateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_internextern", values = {
		@DSValueSetValue(code = "260521003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "261074009", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "157", displayName = "Consult materiaal aangevraagd", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.429']]]/hl7:act/hl7:code[(@code='726007' and @codeSystem='2.16.840.1.113883.6.96')]/hl7:qualifier/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.429']]]/hl7:act/hl7:code[(@code='726007' and @codeSystem='2.16.840.1.113883.6.96')]/hl7:qualifier/hl7:value|@code"
	})
	private DSValue consultMateriaalAangevraagd;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_neoplasiegraad", values = {
		@DSValueSetValue(code = "399611001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "399415002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "158", displayName = "Dysplasiegraad", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.401']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.401']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue dysplasiegraad;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "diameterPoliepValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "diameterPoliepUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "diameterPoliepNf"))
	})
	@VraagElement(conceptId = "160", displayName = "Diameter poliep", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.402']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.402']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private NullFlavourQuantity diameterPoliep;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_primaire_afwijking", values = {
		@DSValueSetValue(code = "23875004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "12402003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443897009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "61722000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "62047007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128859003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443734007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "443157008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "783210009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTHPOL", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.13"),
		@DSValueSetValue(code = "76235005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "110448004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "277161008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "46720004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "44598004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "80297003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "53801007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "89084002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "12169001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "128795001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "24183004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "281268007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "125154007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "400110009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "32110003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "123827008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373379001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "162572001:246090004=269533000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "64226004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "MESPOL", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.13", deprecated = true),
		@DSValueSetValue(code = "MUCPOL", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.13", deprecated = true),
		@DSValueSetValue(code = "269533000:408729009=415684004", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "162", displayName = "Primaire afwijking", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.403']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.403']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue primaireAfwijking;

	@Column(length = 4096)
	@VraagElement(conceptId = "163", displayName = "Andere primaire afwijking", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.403']]]/hl7:observation/hl7:text",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.403']]]/hl7:observation/hl7:text"
	})
	private String anderePrimaireAfwijking;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_pa_bevinding", values = {
		@DSValueSetValue(code = "110396000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373379001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "395705003", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "162572001:246090004=269533000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "164", displayName = "Bevinding", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.404']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.404']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue bevinding;

	@Column(length = 4096)
	@VraagElement(conceptId = "165", displayName = "Specificatie overige bevinding", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.404']]]/hl7:observation/hl7:text",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.211']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.551']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.404']]]/hl7:observation/hl7:text"
	})
	private String specificatieOverigeBevinding;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "pathologieProtocolColonbioptperPoliep", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "177", displayName = "Gegevens bij maligniteit")
	private PaGegevensBijMaligniteit gegevensBijMaligniteit;

}
