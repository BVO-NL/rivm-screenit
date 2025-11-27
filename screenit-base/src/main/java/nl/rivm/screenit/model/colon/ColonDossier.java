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

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.colon.enums.InactiveerReden;

import org.hibernate.annotations.Cascade;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(schema = "colon", name = "dossier")
@Audited
@Getter
@Setter
public class ColonDossier extends Dossier<ColonScreeningRonde, ColonAfmelding>
{
	@OneToOne(mappedBy = "colonDossier", optional = false)
	@JsonBackReference
	private Client client;

	@Enumerated(EnumType.STRING)
	private InactiveerReden inactiveerReden;

	@OneToOne(cascade = CascadeType.ALL)
	@NotAudited
	private ColonVooraankondiging vooraankondiging;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "dossier", cascade = CascadeType.ALL)
	private List<ColonScreeningRonde> screeningRondes = new ArrayList<>();

	@OneToOne(cascade = CascadeType.ALL)
	private ColonScreeningRonde laatsteScreeningRonde;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "dossier")
	private List<ColonAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(cascade = CascadeType.ALL)
	private ColonAfmelding laatsteAfmelding;

	@OneToOne(mappedBy = "dossier", fetch = FetchType.LAZY, cascade = jakarta.persistence.CascadeType.REMOVE)
	@Cascade(org.hibernate.annotations.CascadeType.DELETE)
	private ColonVolgendeUitnodiging volgendeUitnodiging;
}
