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
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.OrderBy;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.RedenIntrekkenGbaIndicatie;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.gba.GbaVraag;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.project.ProjectClient;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.BsnBron;

import org.hibernate.annotations.Check;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import com.fasterxml.jackson.annotation.JsonManagedReference;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Getter
@Setter
@Audited
@Entity(name = "pat_patient")
@Table(schema = "gedeeld", indexes = { @Index(name = "IDX_GBASTATUS", columnList = "gbaStatus") })
@Check(constraints = "reden_intrekken_gba_indicatie_door_bvo = 'NIET_INGETROKKEN' OR gba_status IN ('INDICATIE_AANWEZIG', 'PUNT_ADRES', 'BEZWAAR')")
public class Client extends AbstractHibernateObject implements Account
{
	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	@JsonManagedReference
	private ColonDossier colonDossier;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	@JsonManagedReference
	private CervixDossier cervixDossier;

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	@JsonManagedReference
	private MammaDossier mammaDossier;

	@OneToMany(mappedBy = "client", fetch = FetchType.LAZY)
	private List<ColonIntakeAfspraak> afspraken = new ArrayList<>();

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private GbaStatus gbaStatus;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private RedenIntrekkenGbaIndicatie redenIntrekkenGbaIndicatieDoorBvo = RedenIntrekkenGbaIndicatie.NIET_INGETROKKEN;

	@OneToMany(cascade = CascadeType.ALL)
	@NotAudited
	@JoinTable(schema = "gedeeld", name = "pat_patient_gba_mutaties", joinColumns = { @JoinColumn(name = "pat_patient") })
	private List<GbaMutatie> gbaMutaties = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	@NotAudited
	private List<GbaVraag> gbaVragen = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	private List<ClientContact> contacten = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	@OrderBy("bezwaarDatum DESC")
	@JsonManagedReference
	private List<BezwaarMoment> bezwaarMomenten = new ArrayList<>();

	@OneToMany(mappedBy = "client")
	@OrderBy("moment DESC")
	private List<OnderzoeksresultatenActie> onderzoeksresultatenActies = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	@JsonManagedReference
	private BezwaarMoment laatstVoltooideBezwaarMoment;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "client")
	@NotAudited
	private List<HuisartsBericht> huisartsBerichten = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY)
	@NotAudited
	@JoinTable(schema = "gedeeld", name = "pat_patient_documents", joinColumns = { @JoinColumn(name = "pat_patient") })
	private List<UploadDocument> documents;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "client")
	@NotAudited
	private List<ProjectClient> projecten = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@Audited(targetAuditMode = NOT_AUDITED)
	private GbaMutatie laatsteGbaMutatie;

	@OneToMany(mappedBy = "client")
	@OrderBy("statusDatum DESC")
	private List<OverdrachtPersoonsgegevens> overdrachtPersoonsgegevensLijst = new ArrayList<>();

	@Getter
	@Setter
	@OneToMany(mappedBy = "client", fetch = FetchType.LAZY)
	private List<AlgemeneBrief> algemeneBrieven = new ArrayList<>();

	@Column(length = 20)
	@Deprecated(forRemoval = true)
	private String patientnummer;

	@Column(length = 255)
	@Deprecated(forRemoval = true)
	private String uitgeverPatientnummer;

	@Temporal(TemporalType.DATE)
	@Deprecated(forRemoval = true)
	private Date inschrijfdatum;

	@OneToOne(mappedBy = "patient", cascade = CascadeType.ALL)
	private GbaPersoon persoon;

	@Enumerated(EnumType.STRING)
	@Deprecated(forRemoval = true)
	private BsnBron bsnBron;
}
