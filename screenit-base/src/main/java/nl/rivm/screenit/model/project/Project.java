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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IBevolkingsonderzoek;
import nl.rivm.screenit.model.INaam;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ProjectParameter;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen", uniqueConstraints = @UniqueConstraint(name = "uc_project_naam", columnNames = "naam"))
@Audited
@Getter
@Setter
public class Project extends AbstractHibernateObject implements INaam, IBevolkingsonderzoek
{
	@Column(nullable = false)
	private String naam;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date startDatum;

	@Column(nullable = false)
	@Temporal(TemporalType.DATE)
	private Date eindDatum;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date eindeInstroom;

	@ElementCollection(fetch = FetchType.LAZY, targetClass = Bevolkingsonderzoek.class)
	@Enumerated(EnumType.STRING)
	@NotAudited
	@CollectionTable(schema = "algemeen", name = "project_bevolkingsonderzoeken")
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken;

	@Enumerated(EnumType.STRING)
	private GroepSelectieType groepSelectieType;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	private Instelling organisatie;

	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToOne(fetch = FetchType.LAZY, optional = false, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	private InstellingGebruiker contactpersoon;

	@NotAudited
	@Cascade({ CascadeType.SAVE_UPDATE })
	@ManyToMany(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@JoinTable(schema = "algemeen", name = "project_medewerkers")
	private List<InstellingGebruiker> medewerkers;

	@NotAudited
	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "algemeen", name = "project_screening_organisaties")
	private List<Instelling> screeningOrganisaties = new ArrayList<Instelling>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project", cascade = jakarta.persistence.CascadeType.ALL)
	private List<ProjectClient> clienten = new ArrayList<>();

	private Boolean excludeerBezwaar = Boolean.FALSE;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "project_excludeer_afmelding")
	private List<Bevolkingsonderzoek> excludeerAfmelding = new ArrayList<>();

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "project_excludeer_open_ronde")
	private List<Bevolkingsonderzoek> excludeerOpenRonde = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project", cascade = jakarta.persistence.CascadeType.ALL)
	private List<ProjectGroep> groepen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project", cascade = jakarta.persistence.CascadeType.ALL)
	private List<ProjectBriefActie> projectBriefActies = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project", cascade = jakarta.persistence.CascadeType.ALL)
	private List<ProjectAttribuut> projectAttributen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "project", orphanRemoval = true, cascade = jakarta.persistence.CascadeType.ALL)
	private List<ProjectParameter> parameters = new ArrayList<>();

	private Boolean anoniem;

	@Column(length = HibernateMagicNumber.L255)
	private String opmerkingen;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private ProjectType type;

	@Transient
	private List<ProjectType> projectTypes = new ArrayList<>();

	@Transient
	private List<ProjectStatus> projectStatussen = new ArrayList<ProjectStatus>();

	@Override
	public Boolean getExactMatch()
	{
		return null; 
	}

	@Override
	public void setExctMatch(Boolean exactMatch)
	{

	}
}
