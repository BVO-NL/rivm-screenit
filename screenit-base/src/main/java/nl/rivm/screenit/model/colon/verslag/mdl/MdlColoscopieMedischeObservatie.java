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
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
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
public class MdlColoscopieMedischeObservatie
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerslagContent verslagContent;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "81", displayName = "Voorbereiding coloscopie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.307']]]/hl7:act",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.307']]]/hl7:act"
	})
	private MdlVoorbereidingColoscopie voorbereidingColoscopie;

	@Column
	@VraagElement(conceptId = "89", displayName = "Time-out procedure doorlopen", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.342']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.203']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.342']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Boolean timeoutProcedureDoorlopen;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "90", displayName = "Medicatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.203']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.502']]]/hl7:organizer"
	})
	private MdlMedicatie medicatie;

	@Column
	@VraagElement(conceptId = "97", displayName = "CO2 insufflatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.322']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.322']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Boolean co2Insufflatie;

	@Column
	@VraagElement(conceptId = "98", displayName = "Coecum / terminaal ileum intubatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.329']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.329']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Boolean coecumTerminaalIleumIntubatie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_reden_coecum_niet_bereikt", values = {
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "13", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.250"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "99", displayName = "Reden coecum niet bereikt", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.343']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.343']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue redenCoecumNietBereikt;

	@Column(length = 4096)
	@VraagElement(conceptId = "100", displayName = "Text reden coecum niet bereikt", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.343']]]/hl7:observation/hl7:value/hl7:originalText",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.343']]]/hl7:observation/hl7:value/hl7:originalText"
	}, isVerplicht = true)
	private String textRedenCoecumNietBereikt;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_lokalisatie_bereikt", values = {
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
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "101", displayName = "Diepste punt insertie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.330']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.330']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue diepstePuntInsertie;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "afstandVanafAnusValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "afstandVanafAnusUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "afstandVanafAnusNf"))
	})
	@VraagElement(conceptId = "102", displayName = "Afstand vanaf anus", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.344']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.344']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private NullFlavourQuantity afstandVanafAnus;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "totaleTerugtrektijdValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "totaleTerugtrektijdUnit")),
		@AttributeOverride(name = "nullFlavour", column = @Column(name = "totaleTerugtrektijdNf"))
	})
	@VraagElement(conceptId = "103", displayName = "Totale terugtrektijd", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.323']]]/hl7:observation/hl7:value[not(@nullFlavor)]|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.323']]]/hl7:observation/hl7:value[not(@nullFlavor)]|@value"
	}, isVerplicht = true)
	private NullFlavourQuantity totaleTerugtrektijd;

	@Column
	@VraagElement(conceptId = "104", displayName = "Retroflexie rectum", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.331']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.331']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Boolean retroflexieRectum;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_gloucestercomfortscore", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.104")
	})
	@VraagElement(conceptId = "105", displayName = "Pati\u00ebntcomfort (GCS)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.328']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.328']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue patientcomfortgcs;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_coloscopie_medische_observatie_reden_afbreking_coloscopie")
	@DSValueSet(name = "vs_afbreken_coloscopie", values = {
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37", deprecated = true),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "11", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "9", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "10", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "12", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.37"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "106", displayName = "Reden afbreking coloscopie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.334']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.334']]]/hl7:observation/hl7:value|@code"
	})
	private List<DSValue> redenAfbrekingColoscopie = new ArrayList<>();

	@Column(length = 4096)
	@VraagElement(conceptId = "107", displayName = "Text reden afbreking coloscopie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.334']]]/hl7:observation/hl7:value/hl7:originalText",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.204']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.320']]]/hl7:act/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.334']]]/hl7:observation/hl7:value/hl7:originalText"
	}, isVerplicht = true)
	private String textRedenAfbrekingColoscopie;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_terkoppeling_coloscopie", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "5", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "6", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "7", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38"),
		@DSValueSetValue(code = "8", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.38")
	})
	@VraagElement(conceptId = "108", displayName = "Terugkoppeling cli\u00ebnt", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.370']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.207']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.370']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue terugkoppelingClient;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "109", displayName = "Definitief vervolgbeleid voor bevolkingsonderzoek (groep)", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.372']]]/hl7:act",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.207']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.372']]]/hl7:act"
	})
	private MdlDefinitiefVervolgbeleidVoorBevolkingsonderzoekg definitiefVervolgbeleidVoorBevolkingsonderzoekg;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "colon", name = "mdl_coloscopie_medische_observatie_overige_bevinding")
	@DSValueSet(name = "vs_overige_bevindingen", values = {
		@DSValueSetValue(code = "255046005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "448315008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "285611007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "64226004", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "3951002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "52457000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "397881000", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "307496006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "70153002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "30037006", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "54609002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "90858003", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "114", displayName = "Overige bevinding", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.208']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.381']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.208']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.381']]]/hl7:observation/hl7:value|@code"
	})
	private List<DSValue> overigeBevinding = new ArrayList<>();

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "totaalAantalGedetecteerdeLaesiesValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "totaalAantalGedetecteerdeLaesiesUnit"))
	})
	@VraagElement(conceptId = "115", displayName = "Totaal aantal gedetecteerde laesies", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.345']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.345']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Quantity totaalAantalGedetecteerdeLaesies;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "aantalVerwijderdeLaesiesNietIngezondenVoorPaEnGValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "aantalVerwijderdeLaesiesNietIngezondenVoorPaEnGUnit"))
	})
	@VraagElement(conceptId = "116", displayName = "Aantal verwijderde laesies niet ingezonden voor PA en gegevens niet vastgelegd", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.346']]]/hl7:observation/hl7:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.346']]]/hl7:observation/hl7:value|@value"
	}, isVerplicht = true)
	private Quantity aantalVerwijderdeLaesiesNietIngezondenVoorPaEnG;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_reasonnotsenttopathology", values = {
		@DSValueSetValue(code = "1", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "2", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "3", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "4", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.224"),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "117", displayName = "Reden poliep(en) niet ingezonden voor pathologie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.346']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.347']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.346']]]/hl7:observation/hl7:entryRelationship[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.347']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue redenPoliepenNietIngezondenVoorPathologie;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "118-group", displayName = "Opdrachtnummer PA-lab", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.332']]]/hl7:act/hl7:id|@extension",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.332']]]/hl7:act/hl7:id|@extension"
	})
	private List<MdlOpdrachtnummerPalab> opdrachtnummerPalab = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "coloscopieMedischeObservatie", cascade = CascadeType.ALL)
	@VraagElement(conceptId = "119-group", displayName = "T-nummer pathologie verslag", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.338']]]/hl7:act/hl7:id|@extension",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.338']]]/hl7:act/hl7:id|@extension"
	})
	private List<MdlTnummerPathologieVerslag> tnummerPathologieVerslag = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "ProfielColoscopie", values = {
		@DSValueSetValue(code = "HRP", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "LRP", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78")
	})
	@VraagElement(conceptId = "13", displayName = "Profiel", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.208']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.900837']]]/hl7:observation/hl7:value|@code"
	})
	private DSValue profiel;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_conclusie_coloscopie", values = {
		@DSValueSetValue(code = "313170008", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "NAAD", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "AAD", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "449855005", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "CRC", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78"),
		@DSValueSetValue(code = "255046005", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "448315008", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "285611007", codeSystem = "2.16.840.1.113883.6.96", deprecated = true),
		@DSValueSetValue(code = "HRP", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78", deprecated = true),
		@DSValueSetValue(code = "LRP", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.78", deprecated = true),
		@DSValueSetValue(code = "OTH", codeSystem = "2.16.840.1.113883.5.1008")
	})
	@VraagElement(conceptId = "120", displayName = "Eindconclusie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.208']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.380']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.208']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.380']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue eindconclusie;

}
