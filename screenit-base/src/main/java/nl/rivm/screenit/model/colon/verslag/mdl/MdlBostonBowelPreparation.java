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
@Table(schema = "colon")
@Getter
@Setter
public class MdlBostonBowelPreparation
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVoorbereidingColoscopie voorbereidingColoscopie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_sumscore", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.103"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "85", displayName = "Boston Bowel Preparation Scale Sum Score", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.310']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.310']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue bostonBowelPreparationScaleSumScore;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_xscale", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "86", displayName = "BBPS Score Colon ascendens", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.310']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.311']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.310']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.311']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue bbpsScoreColonAscendens;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_xscale", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "87", displayName = "BBPS Score Colon transversum", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.310']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.312']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.310']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.312']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue bbpsScoreColonTransversum;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_BBPS_xscale", values = {
		@DSValueSetValue(code = "0", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.102"),
		@DSValueSetValue(code = "ASKU", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "88", displayName = "BBPS Score Colon descendens", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.310']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.313']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.307']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.310']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.313']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue bbpsScoreColonDescendens;

}
