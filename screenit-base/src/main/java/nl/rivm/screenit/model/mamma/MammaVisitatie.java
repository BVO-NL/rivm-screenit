package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaFotorichting;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieStatus;
import nl.rivm.screenit.util.DiffSpecs;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "mamma", name = "visitatie")
@Audited
@Getter
@Setter
public class MammaVisitatie extends AbstractHibernateObject implements MammaIKwaliteitscontrole
{
	@Column(nullable = false, length = HibernateMagicNumber.L256, unique = true)
	private String omschrijving;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date aangemaaktOp;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@NotAudited
	private OrganisatieMedewerker aangemaaktDoor;

	@ManyToOne(fetch = FetchType.LAZY)
	private BeoordelingsEenheid beoordelingsEenheid;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date gestartOp;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "visitatie", cascade = { jakarta.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	private List<MammaVisitatieOnderzoek> onderzoeken = new ArrayList<>();

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date afgerondOp;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaVisitatieStatus status;

	@OneToOne(fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private UploadDocument rapportageBijlage;

	@OneToOne(fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private UploadDocument vragenlijstBijlage;

	@Column
	@Enumerated(EnumType.STRING)
	private MammaFotorichting fotorichting;
}
