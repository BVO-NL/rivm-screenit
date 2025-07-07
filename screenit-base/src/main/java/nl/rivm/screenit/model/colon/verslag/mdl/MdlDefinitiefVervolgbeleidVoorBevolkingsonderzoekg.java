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
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlColoscopieMedischeObservatie coloscopieMedischeObservatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_vervolgbeleid", values = {
		@DSValueSetValue(code = "183851006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "311774002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "410410006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "308535007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "73761001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "73761001:260870009=64695001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "418714002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "428119001:363589002=73761001", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "183654001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "73761001:408730004=64695001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "410410006:408730004=428119001", codeSystem = "2.16.840.1.113883.6.96", deprecated = true)
	})
	@VraagElement(conceptId = "110", displayName = "Definitief vervolgbeleid voor bevolkingsonderzoek", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.372']]]/hl7:act/hl7:code|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.372']]]/hl7:act/hl7:code|@code"
	}, isVerplicht = true)
	private DSValue definitiefVervolgbeleidVoorBevolkingsonderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_periode_vervolg", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "14", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226")
	})
	@VraagElement(conceptId = "15", displayName = "Periode vervolg scopie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.372']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:code/@code = 'SCOPEPERIOD']]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.372']]]/hl7:act/hl7:effectiveTime/hl7:width|@value"
	})
	private DSValue periodeVervolgScopie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_periode_vervolg_surveillance", values = {
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "13", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "14", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226"),
		@DSValueSetValue(code = "15", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.226")
	})
	@VraagElement(conceptId = "14", displayName = "Periode vervolg surveillance", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.372']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:code/@code = 'SCOPESURV']]/hl7:observation/hl7:value|@code"
	})
	private DSValue periodeVervolgSurveillance;

	@Column(length = 4096)
	@VraagElement(conceptId = "112", displayName = "Locatie vervolgscopie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.372']]]/hl7:act/hl7:performer/hl7:assignedEntity/hl7:representedOrganization/hl7:name",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.372']]]/hl7:act/hl7:performer/hl7:assignedEntity/hl7:representedOrganization/hl7:name"
	})
	private String locatieVervolgscopie;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "adenoomRiskScoretotalValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "adenoomRiskScoretotalUnit"))
	})
	@VraagElement(conceptId = "113", displayName = "Adenoom risk score (total)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.372']]]/hl7:act/hl7:entryRelationship/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.372']]]/hl7:act/hl7:entryRelationship/hl7:observation/hl7:value|@value"
	})
	private Quantity adenoomRiskScoretotal;

}
