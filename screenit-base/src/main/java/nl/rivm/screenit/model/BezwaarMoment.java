package nl.rivm.screenit.model;

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
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Getter
@Setter
@Table(schema = "algemeen")
@Audited
public class BezwaarMoment extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	@JsonBackReference
	private Client client;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AanvraagBriefStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@NotAudited
	private UploadDocument bezwaarBrief;

	private Date bezwaarDatum;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "bezwaarMoment")
	private List<BezwaarBrief> brieven = new ArrayList<BezwaarBrief>();

	@NotAudited
	@OneToMany(cascade = CascadeType.ALL, mappedBy = "bezwaarMoment", fetch = FetchType.LAZY)
	@JsonManagedReference
	private List<Bezwaar> bezwaren = new ArrayList<Bezwaar>();

	@Transient
	private ClientContactManier manier;
}
