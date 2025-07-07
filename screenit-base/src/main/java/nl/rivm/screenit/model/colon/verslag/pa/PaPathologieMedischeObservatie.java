package nl.rivm.screenit.model.colon.verslag.pa;

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

import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "colon")
@Getter
@Setter
public class PaPathologieMedischeObservatie
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private PaVerslagContent verslagContent;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "148", displayName = "Datum ontvangst materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:effectiveTime/hl7:low|@value",
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:effectiveTime/hl7:low|@value"
	}, isVerplicht = true)
	private Date datumOntvangstMateriaal;

	@Column(length = 255)
	@VraagElement(conceptId = "149", displayName = "T-nummer laboratorium", xpaths = {
		"/hl7:ClinicalDocument/hl7:setId|@extension",
		"/hl7:ClinicalDocument/hl7:setId/@extension"
	}, isVerplicht = true)
	private String tnummerLaboratorium;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "150", displayName = "Datum autorisatie uitslag", xpaths = {
		"/hl7:ClinicalDocument/hl7:effectiveTime|@value",
		"/hl7:ClinicalDocument/hl7:effectiveTime|@value"
	}, isVerplicht = true)
	private Date datumAutorisatieUitslag;

	@Column(length = 4096)
	@VraagElement(conceptId = "151", displayName = "Versie protocol", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.137.10.430']]]/hl7:observation/hl7:value",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.212']]]/hl7:section/hl7:entry[hl7:observation[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.430']]]/hl7:observation/hl7:value"
	}, isVerplicht = true)
	private String versieProtocol;

}
