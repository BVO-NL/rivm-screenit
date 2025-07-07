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
@Table(schema = "cervix")
@Getter
@Setter
public class CervixCytologieMonsterBmhk
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk;

	@Column(length = 255)
	@VraagElement(conceptId = "248", displayName = "Monster identificatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:participant/hl7:participantRole/hl7:id|@extension",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:participant/hl7:participantRole/hl7:id|@extension"
	}, isVerplicht = true)
	private String monsterIdentificatie;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "249", displayName = "Datum afname materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:effectiveTime",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:effectiveTime"
	}, isVerplicht = true)
	private Date datumAfnameMateriaal;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "250", displayName = "Datum ontvangst materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:effectiveTime/hl7:low|@value",
		"/hl7:ClinicalDocument/hl7:documentationOf/hl7:serviceEvent/hl7:effectiveTime/hl7:low|@value"
	}, isVerplicht = true)
	private Date datumOntvangstMateriaal;

	@ManyToOne(fetch = FetchType.LAZY)
	@DSValueSet(name = "vs_type_monster", values = {
		@DSValueSetValue(code = "308728002", codeSystem = "2.16.840.1.113883.6.96"),
		@DSValueSetValue(code = "ZAS", codeSystem = "2.16.840.1.113883.2.4.3.36.77.5.241")
	})
	@VraagElement(conceptId = "251", displayName = "Type materiaal", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:code|@code",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.213']]]/hl7:section/hl7:entry[hl7:organizer[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.384']]]/hl7:organizer/hl7:component/hl7:procedure/hl7:code|@code"
	}, isVerplicht = true)
	private DSValue typeMateriaal;

	@Temporal(TemporalType.DATE)
	@Column
	@VraagElement(conceptId = "252", displayName = "Datum autorisatie", xpaths = {
		"/hl7:ClinicalDocument/hl7:effectiveTime|@value",
		"/hl7:ClinicalDocument/hl7:effectiveTime|@value"
	}, isVerplicht = true)
	private Date datumAutorisatie;

}
