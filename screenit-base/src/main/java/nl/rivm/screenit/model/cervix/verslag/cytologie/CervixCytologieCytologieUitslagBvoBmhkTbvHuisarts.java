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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.verslag.VraagElement;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "cervix")
@Getter
@Setter
public class CervixCytologieCytologieUitslagBvoBmhkTbvHuisarts
	extends AbstractHibernateObject
{

	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	private CervixCytologieVerslagContent verslagContent;

	@Column(length = 4096)
	@VraagElement(conceptId = "267", displayName = "Protocollair verslag", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.214']]]/hl7:section/hl7:text",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.214']]]/hl7:section/hl7:text"
	}, isVerplicht = true)
	private String protocollairVerslag;

	@Column(length = 4096)
	@VraagElement(conceptId = "268", displayName = "Conclusie", xpaths = {
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.11.60.142.10.212']]]/hl7:section/hl7:text",
		"/hl7:ClinicalDocument/hl7:component/hl7:structuredBody/hl7:component[hl7:section[hl7:templateId[@root='2.16.840.1.113883.2.4.3.36.10.212']]]/hl7:section/hl7:text"
	}, isVerplicht = true)
	private String conclusie;

}
