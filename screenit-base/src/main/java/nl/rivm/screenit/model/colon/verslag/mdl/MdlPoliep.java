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

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.verslag.DSValue;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.DSValueSetValue;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class MdlPoliep
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlLaesiecoloscopiecentrum laesiecoloscopiecentrum;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "diameterPoliepValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "diameterPoliepUnit"))
	})
	@VraagElement(conceptId = "133", displayName = "Diameter poliep", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='384627007']/lab:observationEvent/lab:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='384627007']/lab:observationEvent/lab:value|@value"
	}, isVerplicht = true)
	private Quantity diameterPoliep;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_morfologie_coloscopie", values = {
		@DSValueSetValue(code = "103680002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103679000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.29"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "134", displayName = "Morfologie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='41329004']/lab:observationEvent/lab:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='41329004']/lab:observationEvent/lab:value|@code"
	}, isVerplicht = true)
	private DSValue morfologie;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_poliep_manier_van_verwijderen")
	@DSValueSet(name = "vs_verwijderingstechniek", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.34"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "135", displayName = "Manier van verwijderen", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:methodCode|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:methodCode|@code"
	}, isVerplicht = true)
	private List<DSValue> manierVanVerwijderen = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_verwijdering_compleet", values = {
		@DSValueSetValue(code = "255619001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.35"),
		@DSValueSetValue(code = "255599008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.35")
	})
	@VraagElement(conceptId = "145090", displayName = "Volledigheid wegname materiaal", isVerplicht = true, useInCda = false)
	private DSValue volledigheidWegnameMateriaal;

	@Column(length = 4096)
	@VraagElement(conceptId = "136", displayName = "Overige manier van verwijderen", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:methodCode/lab:originalText",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:methodCode/lab:originalText"
	}, isVerplicht = true)
	private String overigeManierVanVerwijderen;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_method_of_excision", values = {
		@DSValueSetValue(code = "255619001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.35")
	})
	@VraagElement(conceptId = "137", displayName = "Methode van verwijderen", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:code/lab:qualifier[lab:name/@code='246386008']/lab:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:code/lab:qualifier[lab:name/@code='246386008']/lab:value|@code"
	}, isVerplicht = true)
	private DSValue methodeVanVerwijderen;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_extent", values = {
		@DSValueSetValue(code = "255612005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "255599008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "138", displayName = "Resultaat verwijdering", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:code/lab:qualifier[lab:name/@code='260858005']/lab:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:code/lab:qualifier[lab:name/@code='260858005']/lab:value|@code"
	}, isVerplicht = true)
	private DSValue resultaatVerwijdering;

}
