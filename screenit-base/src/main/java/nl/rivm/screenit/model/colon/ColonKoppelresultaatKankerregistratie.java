package nl.rivm.screenit.model.colon;

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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.AbstractKoppelresultaatKankerregistratie;
import nl.rivm.screenit.model.UploadDocument;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "colon", name = "koppelresultaat_kankerregistratie")
@Getter
@Setter
public class ColonKoppelresultaatKankerregistratie extends AbstractKoppelresultaatKankerregistratie<ColonScreeningRonde>
{
	@Temporal(TemporalType.DATE)
	@Column(nullable = false)
	private Date incidentiedatum;

	@Column(nullable = false)
	private Integer eid;

	private String codeZiekenhuisVanDiagnose;

	private String omschrijvingZiekenhuisVanDiagnose;

	private String redenDiagnose;

	private String topografie;

	private String morfologie;

	private String tumorgedrag;

	private String differentiatiegraad;

	private String cTNM;

	private String pTNM;

	private String ypTNM;

	@ManyToOne(optional = false, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private UploadDocument uploadedFile;

	@ManyToOne(optional = false, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private ColonScreeningRonde screeningsRonde;
}
