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

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Getter
@Setter
@Table(schema = "algemeen")
@Audited
public class ProjectBriefActie extends AbstractHibernateObject implements IActief, IDocument
{
	@Column(nullable = false)
	private Boolean actief = true;

	@Column
	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@ManyToOne(fetch = FetchType.LAZY)
	private Project project;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private ProjectBriefActieType type;

	@Column(nullable = true)
	private Integer aantalDagen;

	@Temporal(TemporalType.DATE)
	private Date datum;

	@ManyToOne(optional = false)
	private UploadDocument document;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date laatstGewijzigd;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private OrganisatieMedewerker uploader;

	@Column
	private String misluktBak;

	@Transient
	private boolean herinneren = false;

	@OneToOne(fetch = FetchType.LAZY)
	private ProjectBriefActie herinneringsActie;

	@OneToOne(mappedBy = "herinneringsActie")
	private ProjectBriefActie baseActie;

	private String formulierNummer;

	private String printomschrijving;

	public ProjectBriefActie()
	{
	}

	public ProjectBriefActie(Project project)
	{
		this.project = project;
	}
}
