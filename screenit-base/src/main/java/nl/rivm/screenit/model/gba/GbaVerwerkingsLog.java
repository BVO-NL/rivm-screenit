package nl.rivm.screenit.model.gba;

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
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;

@Getter
@Setter
@Entity
@Table(schema = "algemeen")
public class GbaVerwerkingsLog extends AbstractHibernateObject
{
	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date datumVerwerking;

	@Column(nullable = false)
	private Integer aantalNieuweBurgers = 0;

	@Column(nullable = false)
	private Integer aantalBijgewerkteBugers = 0;

	@Column
	private Long aantalNieuweColonDossiers = 0L;

	@Column
	private Long aantalNieuweCervixDossiers = 0L;

	@Column
	private Long aantalNieuweMammaDossiers = 0L;

	@OneToMany(mappedBy = "verwerkingsLog", cascade = CascadeType.REMOVE)
	@Cascade(org.hibernate.annotations.CascadeType.DELETE)
	private List<GbaVerwerkingEntry> entries = new ArrayList<>();

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "verwerkingsLog")
	private List<GbaFoutRegel> fouten = new ArrayList<>();

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "gbaVerwerkingsLog")
	private List<GbaFile> bestanden = new ArrayList<>();
}
