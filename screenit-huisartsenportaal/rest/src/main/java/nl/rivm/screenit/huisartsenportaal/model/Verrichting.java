package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.envers.Audited;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

@Entity
@Audited
@Table(name = "verrichting", indexes = { @Index(name = "idx_VERRICHTING_VERRICHTINGS_DATUM", columnList = "verrichtingsDatum") })
@Getter
@Setter
public class Verrichting extends AbstractReferenceObject
{
	@Column(nullable = false)
	private String regio;

	@Column(nullable = false)
	private String monsterId;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date verrichtingsDatum;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date datumUitstrijkje;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date formulierOntvangstDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Locatie huisartsLocatie;

	@ManyToOne(fetch = FetchType.LAZY)
	private Huisarts huisarts;

	@Column(nullable = false)
	private String clientNaam;

	@OneToMany(mappedBy = "verrichting", fetch = FetchType.LAZY)
	private List<Betaling> betalingen = new ArrayList<>();
}
