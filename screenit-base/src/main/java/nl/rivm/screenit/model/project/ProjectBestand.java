package nl.rivm.screenit.model.project;

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
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
@Table(schema = "algemeen")
@Getter
@Setter
public class ProjectBestand extends AbstractHibernateObject
{

	@OneToOne(optional = false, fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.SAVE_UPDATE })
	private UploadDocument uploadDocument;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date uploadDatum;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(optional = false, fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	private Project project;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(optional = true, fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	private ProjectGroep groep;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BestandStatus status;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private ProjectBestandType type;

	private boolean populatie = false;

	private boolean attributen = false;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, optional = true, mappedBy = "projectBestand", cascade = jakarta.persistence.CascadeType.ALL)
	private ProjectBestandVerwerking verwerking;

	@Column(nullable = true)
	private String dynamischeInactiveerReden;

	@Transient
	private String toepassenOp;
}
