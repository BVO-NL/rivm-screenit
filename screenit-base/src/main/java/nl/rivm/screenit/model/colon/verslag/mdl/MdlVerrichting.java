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

import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

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
public class MdlVerrichting
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private MdlVerslagContent verslagContent;

	@Column(length = 255)
	@VraagElement(conceptId = "69", displayName = "Identificatie onderzoek", xpaths = {
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:id|@extension",
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:id|@extension"
	}, isVerplicht = true)
	private String identificatieOnderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_procedureindication", values = {
		@DSValueSetValue(code = "444783004:246513007=261423007", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "444783004:246513007=134433005", codeSystem = "2.16.840.1.113883.6.96")
	})
	@VraagElement(conceptId = "70", displayName = "Indicatie verrichting", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.202']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.136.10.382']]]/hl7:observation/hl7:value|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.202']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.382']]]/hl7:observation/hl7:value|@code"
	}, isVerplicht = true)
	private DSValue indicatieVerrichting;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "72", displayName = "Aanvang verrichting", xpaths = {
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:effectiveTime/hl7:low|@value",
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:effectiveTime/hl7:low|@value"
	}, isVerplicht = true)
	private Date aanvangVerrichting;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "79", displayName = "Autorisatiedatum verslag", xpaths = {
		"/hl7:ClinicalDocument/hl7:legalAuthenticator/hl7:time|@value",
		"/hl7:ClinicalDocument/hl7:legalAuthenticator/hl7:time|@value"
	}, isVerplicht = true)
	private Date autorisatiedatumVerslag;

}
