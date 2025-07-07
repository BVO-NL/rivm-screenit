package nl.rivm.screenit.model.mamma;

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
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Setter
@Getter
@Entity
@Table(schema = "mamma", name = "signaleren")
@Audited
public class MammaSignaleren extends AbstractHibernateObject
{
	@OneToOne(optional = false, mappedBy = "signaleren", fetch = FetchType.LAZY)
	private MammaOnderzoek onderzoek;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE, jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding rechtsVerticaleDoorsnede;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE, jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding linksVerticaleDoorsnede;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE, jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding rechtsHorizontaleDoorsnede;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE, jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaAnnotatieAfbeelding linksHorizontaleDoorsnede;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	private InstellingGebruiker afgerondDoor;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date afgerondOp;

	@Column(nullable = false)
	private boolean heeftAfwijkingen;

}
