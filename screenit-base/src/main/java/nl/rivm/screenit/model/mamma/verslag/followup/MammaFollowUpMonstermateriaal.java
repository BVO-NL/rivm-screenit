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
public class MammaFollowUpMonstermateriaal
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MammaFollowUpFollowupPa followupPa;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_verkrijgingswijze_bk", values = {
		@DSValueSetValue(code = "129249002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "129300006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "65801008", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300010", displayName = "Verkrijgingswijze", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:productOf/lab:specimenCollectionProcess/lab:code|@code"
	}, isVerplicht = true)
	private DSValue verkrijgingswijze;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_locatie_PA_bk", values = {
		@DSValueSetValue(code = "76365002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "33564002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OHT-10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "77831004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "19100000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OHT-11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "24142002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245525003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OHT-12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OHT-13", codeSystem = "2.16.840.1.113883.2.4.3.36.77.0.2.5.1"),
		@DSValueSetValue(code = "OTH-14", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "245526002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "UNK", codeSystem = "2.16.840.1.113883.5.1008"),
		@DSValueSetValue(code = "NAV", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "300120", displayName = "Locatie (topologie)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:productOf/lab:specimenCollectionProcess/lab:targetSiteCode"
	}, isVerplicht = false)
	private DSValue locatietopologie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_clockface_positions_bk", values = {
		@DSValueSetValue(code = "260318004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260328008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260330005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260333007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260335000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260337008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260339006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260341007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260343005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260322009", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260324005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "260326007", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300130", displayName = "Locatie (uren)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:productOf/lab:specimenCollectionProcess/lab:targetSiteCode"
	}, isVerplicht = false)
	private DSValue locatieuren;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_zijdigheid_bk", values = {
		@DSValueSetValue(code = "7771000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "24028007", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "300110", displayName = "Zijdigheid", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.216']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.592']]]/hl7:act/hl7:specimen/hl7:specimenRole/hl7:specimenPlayingEntity/lab:productOf/lab:specimenCollectionProcess/lab:targetSiteCode/hl7:qualifier[hl7:name/@code='20228-3']/hl7:value"
	}, isVerplicht = true)
	private DSValue zijdigheid;

}
