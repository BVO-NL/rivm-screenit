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
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class MdlLaesiecoloscopiecentrum
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerslagContent verslagContent;

	@Column(length = 255)
	@VraagElement(conceptId = "122", displayName = "Nummer potje monster", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:asSpecimenInContainer/lab:container/lab:id|@extension",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:asSpecimenInContainer/lab:container/lab:id|@extension"
	}, isVerplicht = true)
	private String nummerPotjeMonster;

	@Column(length = 255)
	@VraagElement(conceptId = "123", displayName = "Monster identificatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:id|@extension",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:id|@extension"
	})
	private String monsterIdentificatie;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "volgnummerLaesieValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "volgnummerLaesieUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "volgnummerLaesieNf"))
	})
	@VraagElement(conceptId = "124", displayName = "Volgnummer laesie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component/hl7:sequenceNumber|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component/hl7:sequenceNumber|@value"
	}, isVerplicht = true)
	private NullFlavourQuantity volgnummerLaesie;

	@Column
	@VraagElement(conceptId = "125", displayName = "Materiaal (laesie) ingezonden voor pathologie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:procedure/lab:code/@code='702389009']/lab:procedure",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:procedure/lab:code/@code='702389009']/lab:procedure"
	})
	private Boolean materiaallaesieIngezondenVoorPathologie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_lokalisatie", values = {
		@DSValueSetValue(code = "181256004", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "32713005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "9040008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245427008", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "48338005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "485005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "24542800", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "245428003", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "72592005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "32622004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "362166007", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "60184004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245429006", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "49832006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "181261002", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "34402009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "53505006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "85774003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "23153004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "41796003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "256874006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245857005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "83856002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "127", displayName = "Lokalisatie laesie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:targetSiteCode|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:productOf/lab:specimenCollectionProcess/lab:targetSiteCode|@code"
	}, isVerplicht = true)
	private DSValue lokalisatieLaesie;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "afstandVanafAnusValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "afstandVanafAnusUnit"))
	})
	@VraagElement(conceptId = "128", displayName = "Afstand vanaf anus", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='371490004']/lab:observationEvent/lab:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='371490004']/lab:observationEvent/lab:value|@value"
	}, isVerplicht = true)
	private Quantity afstandVanafAnus;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_monstermateriaal", values = {
		@DSValueSetValue(code = "309226005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "258415003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "NA", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "129", displayName = "Type afgenomen materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/hl7:code|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/hl7:code|@code"
	}, isVerplicht = true)
	private DSValue typeAfgenomenMateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_klindiag_coloscopie", values = {
		@DSValueSetValue(code = "89452002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "428054006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "67401000119103:116676008=61647009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "449855005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "408645001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "76235005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "277163006", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "82375006", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.30", deprecated = true),
		@DSValueSetValue(code = "128653004", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "35917007", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "130", displayName = "Klinische diagnose", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='DX']/lab:observationEvent/lab:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='DX']/lab:observationEvent/lab:value|@code"
	})
	private DSValue klinischeDiagnose;

	@Column(length = 4096)
	@VraagElement(conceptId = "131", displayName = "Overige klinische diagnose (tekst)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='DX']/lab:observationEvent/lab:text",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='DX']/lab:observationEvent/lab:text"
	})
	private String overigeKlinischeDiagnosetekst;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "laesiecoloscopiecentrum", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "132", displayName = "Poliep", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity"
	})
	private MdlPoliep poliep;

	@Column
	@VraagElement(conceptId = "139", displayName = "Verdenking carcinoom ja/nee", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='269533000']/lab:observationEvent",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='269533000']/lab:observationEvent"
	}, isVerplicht = true)
	private Boolean verdenkingCarcinoomJanee;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "laesiecoloscopiecentrum", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "140", displayName = "(Verdenking van) carcinoom", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='269533000']/lab:observationEvent",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='269533000']/lab:observationEvent"
	})
	private MdlVanCarcinoom vanCarcinoom;

	@Column
	@VraagElement(conceptId = "146", displayName = "Markering geplaatst", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='14792009']/lab:observationEvent/lab:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='14792009']/lab:observationEvent/lab:value|@value"
	})
	private Boolean markeringGeplaatst;

}
