package nl.rivm.screenit.model.mamma.verslag.followup;

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

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
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
@Table(schema = "mamma")
@Getter
@Setter
public class MammaFollowUpPtnmEnGradering
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MammaFollowUpFollowupPa followupPa;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_pTNM_stage_bk", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IA", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IB", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IIA", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IIB", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IIIA", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IIIB", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IIIC", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "IV", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "300105", displayName = "pTNM (breast) gradering", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.598']]]/hl7:observation/hl7:value"
	})
	private DSValue ptnmbreastGradering;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "UICC-t-value-8ed_bk", values = {
		@DSValueSetValue(code = "Ta", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "Tis", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "Tis(DCIS)", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "Tis(LCIS)", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "Tis(Paget)", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T0", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T1", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T1mi", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T1a", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T1b", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T1c", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T2", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T3", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T4", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T4a", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T4b", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T4c", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "T4d", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "TX", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "300080", displayName = "pT", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.598']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.599']]]/hl7:observation/hl7:value"
	})
	private DSValue pt;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "UICC_n_value_8ed_bk", values = {
		@DSValueSetValue(code = "N0", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N1", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N1mi", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N1a", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N1b", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N1c", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N2", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N2a", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N2b", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N3", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N3a", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N3b", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "N3c", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "NX", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "300090", displayName = "pN", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.598']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.600']]]/hl7:observation/hl7:value"
	})
	private DSValue pn;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "UICC-m-values-8ed_bk", values = {
		@DSValueSetValue(code = "M0", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "M1", codeSystem = "2.16.840.1.113883.15.16"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "300100", displayName = "pM", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.598']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.601']]]/hl7:observation/hl7:value"
	})
	private DSValue pm;

}
