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

import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class MdlVanCarcinoom
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlLaesiecoloscopiecentrum laesiecoloscopiecentrum;

	@Column
	@VraagElement(conceptId = "141", displayName = "Stenoserend ja/nee", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='19132000']/lab:observationEvent",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='19132000']/lab:observationEvent"
	})
	private Boolean stenoserendJanee;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "value", column = @Column(name = "specifiekeAfstandTumorVanafAnusValue")),
		@AttributeOverride(name = "unit", column = @Column(name = "specifiekeAfstandTumorVanafAnusUnit"))
	})
	@VraagElement(conceptId = "144", displayName = "Specifieke afstand tumor vanaf anus", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='371490004']/lab:observationEvent/lab:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='371490004']/lab:observationEvent/lab:value|@value"
	}, isVerplicht = true)
	private Quantity specifiekeAfstandTumorVanafAnus;

	@Column
	@VraagElement(conceptId = "145", displayName = "Te passeren ja/nee", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='C145140']/lab:observationEvent/lab:value|@value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.205']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.383']]]/hl7:organizer/hl7:component[hl7:act[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.333']]]/hl7:act/hl7:specimen/hl7:specimenRole/lab:subjectOf[lab:observationEvent/lab:code/@code='C145140']/lab:observationEvent/lab:value|@value"
	})
	private Boolean tePasserenJanee;

}
