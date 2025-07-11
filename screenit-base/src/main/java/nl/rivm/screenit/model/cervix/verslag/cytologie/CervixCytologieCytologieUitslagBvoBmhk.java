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

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
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
@Table(schema = "cervix")
@Getter
@Setter
public class CervixCytologieCytologieUitslagBvoBmhk
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieVerslagContent verslagContent;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "cytologieUitslagBvoBmhk", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "247", displayName = "Monster BMHK", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:participant",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:participant"
	})
	private CervixCytologieMonsterBmhk monsterBmhk;

	@Column(length = 255)
	@VraagElement(conceptId = "253", displayName = "C-nummer laboratorium", xpaths = {
		"/hl7:ClinicalDocument/hl7:setId/@extension",
		"/hl7:ClinicalDocument/hl7:setId/@extension"
	}, isVerplicht = true)
	private String cnummerLaboratorium;

	@Column(length = 255)
	@VraagElement(conceptId = "254", displayName = "Versie protocol", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.430']]]/hl7:observation/hl7:value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.430']]]/hl7:observation/hl7:value"
	}, isVerplicht = true)
	private String versieProtocol;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Kompositie", values = {
		@DSValueSetValue(code = "K0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231"),
		@DSValueSetValue(code = "K8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.231")
	})
	@VraagElement(conceptId = "255", displayName = "KOPAC-B: Kompositie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.432']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.432']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue kopacbKompositie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Ontstekingsverschijnselen", values = {
		@DSValueSetValue(code = "O0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232"),
		@DSValueSetValue(code = "O9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.232")
	})
	@VraagElement(conceptId = "256", displayName = "KOPAC-B: Ontstekingsverschijnselen", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.433']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.433']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue kopacbOntstekingsverschijnselen;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Plaveiselepitheel", values = {
		@DSValueSetValue(code = "P0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233"),
		@DSValueSetValue(code = "P9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.233")
	})
	@VraagElement(conceptId = "257", displayName = "KOPAC-B: Plaveiselepitheel", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.434']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.434']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue kopacbPlaveiselepitheel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Andere_afwijkingen", values = {
		@DSValueSetValue(code = "A0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234"),
		@DSValueSetValue(code = "A9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.234")
	})
	@VraagElement(conceptId = "258", displayName = "KOPAC-B: Andere afwijkingen / endometrium", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.435']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.435']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue kopacbAndereAfwijkingenEndometrium;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Cilindercelepitheel", values = {
		@DSValueSetValue(code = "C0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235"),
		@DSValueSetValue(code = "C9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.235")
	})
	@VraagElement(conceptId = "259", displayName = "KOPAC-B: Cilindercelepitheel", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.436']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.436']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue kopacbCilindercelepitheel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_Beoordeelbaarheid", values = {
		@DSValueSetValue(code = "B1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.236"),
		@DSValueSetValue(code = "B3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.236")
	})
	@VraagElement(conceptId = "260", displayName = "KOPAC-B: Beoordeelbaarheid", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.437']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.437']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue kopacbBeoordeelbaarheid;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "cervix", name = "cervix_cytologie_cytologie_uitslag_bvo_bmhk_kopacb_extra")
	@DSValueSet(name = "vs_KOPAC_B_Extra", values = {
		@DSValueSetValue(code = "E03", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E05", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E14", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E15", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E18", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238"),
		@DSValueSetValue(code = "E23", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.238")
	})
	@VraagElement(conceptId = "261", displayName = "KOPAC-B: Extra", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.438']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.438']]]/hl7:observation/hl7:value|@code"
	})
	private List<DSValue> kopacbExtra = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_KOPAC_B_reden_niet_beoordeelbaar", values = {
		@DSValueSetValue(code = "B3a", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3b", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3c", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3d", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3e", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3f", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3g", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237"),
		@DSValueSetValue(code = "B3h", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.237")
	})
	@VraagElement(conceptId = "262", displayName = "Reden onbeoordeelbaar B3", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.437']]]/hl7:observation/hl7:entryRelationship/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.437']]]/hl7:observation/hl7:entryRelationship/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue redenOnbeoordeelbaarB3;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_PAPKlasse", values = {
		@DSValueSetValue(code = "Pap0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap3a1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap3a2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap3b", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239"),
		@DSValueSetValue(code = "Pap5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.239")
	})
	@VraagElement(conceptId = "263", displayName = "PAP klasse", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.439']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.439']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue papKlasse;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "cervix", name = "cervix_cytologie_cytologie_uitslag_bvo_bmhk_bethesda_score")
	@DSValueSet(name = "vs_Bethesda", values = {
		@DSValueSetValue(code = "373887005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "168402006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "39035006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103639009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "112662005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "22725004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "103648004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373883009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "373878001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "51642000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "88400008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "264", displayName = "Bethesda score", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.440']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.558']]]/hl7:organizer/hl7:component/hl7:organizer/hl7:component[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.440']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private List<DSValue> bethesdaScore = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_adviesBMHK", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.101")
	})
	@VraagElement(conceptId = "265", displayName = "Screeningsadvies / herhaling", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.215']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.385']]]/hl7:act/hl7:code|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.215']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.385']]]/hl7:act/hl7:code|@code"
	}, isVerplicht = true)
	private DSValue screeningsadviesHerhaling;

	@Column
	@VraagElement(conceptId = "275", displayName = "COS", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:code[(@code='COS' and @codeSystem='2.16.840.1.113883.2.4.3.36.77.5.266')]]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:code[(@code='COS' and @codeSystem='2.16.840.1.113883.2.4.3.36.77.5.266')]]]/hl7:observation/hl7:value|@value"
	})
	private Boolean cos;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_COSPlatform", values = {
		@DSValueSetValue(code = "TIS", codeSystem = "2.16.840.1.113883.2.4.3.36.77.11.267"),
		@DSValueSetValue(code = "FPGIS", codeSystem = "2.16.840.1.113883.2.4.3.36.77.11.267")
	})
	@VraagElement(conceptId = "276", displayName = "COS platform", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:code[(@code='COS' and @codeSystem='2.16.840.1.113883.2.4.3.36.77.5.266')]]]/hl7:observation/hl7:entryRelationship/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:code[(@code='COS' and @codeSystem='2.16.840.1.113883.2.4.3.36.77.5.266')]]]/hl7:observation/hl7:entryRelationship/hl7:observation/hl7:value|@code"
	})
	private DSValue cosPlatform;

}
