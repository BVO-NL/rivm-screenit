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

import java.io.Serial;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.enums.AanduidingBijzonderNederlanderschap;
import nl.rivm.screenit.model.enums.DatumPrecisie;
import nl.rivm.screenit.model.enums.IndicatieGeheim;
import nl.rivm.screenit.model.gba.Land;
import nl.rivm.screenit.model.gba.Nationaliteit;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.BurgelijkeStaat;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.DocumentType;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Polis;

import org.hibernate.envers.AuditJoinTable;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@NoArgsConstructor
@Entity(name = "pat_persoon")
@Table(schema = "gedeeld",
	indexes = { @Index(name = "pat_per_geboortedatumIndex", columnList = "geboortedatum") },
	uniqueConstraints = { @UniqueConstraint(columnNames = "bsn"), @UniqueConstraint(columnNames = "anummer") })
@Audited

public class GbaPersoon extends AbstractHibernateObject
{
	@Serial
	private static final long serialVersionUID = 1L;

	public static final int MAX_EMAIL_LENGTH = 100;

	public static final int MAX_PHONE_LENGTH = 20;

	private static final Date BUGDATUM; 

	static
	{
		BUGDATUM = DateUtil.parseDateForPattern("30-05-1937 23:59:32", Constants.DEFAULT_DATE_TIME_SECONDS_FORMAT);
	}

	@Column
	private String anummer;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH })
	private BagAdres gbaAdres;

	@OneToOne(cascade = CascadeType.ALL)
	private TijdelijkAdres tijdelijkAdres;

	@OneToOne(cascade = CascadeType.ALL)
	private TijdelijkGbaAdres tijdelijkGbaAdres;

	@Deprecated(forRemoval = true)
	@ManyToMany
	@NotAudited
	@JoinTable(schema = "gedeeld", name = "pat_persoon_gba_nationaliteiten", joinColumns = { @JoinColumn(name = "pat_persoon") })
	private List<Nationaliteit> gbaNationaliteiten = new ArrayList<>();

	@Deprecated(forRemoval = true)
	@ManyToOne
	@NotAudited
	private Land gbaGeboorteLand;

	@Temporal(TemporalType.DATE)
	private Date datumVertrokkenUitNederland;

	@Enumerated(EnumType.STRING)
	private DatumPrecisie geboortedatumPrecisie = DatumPrecisie.VOLLEDIG;

	private String titelCode;

	@Enumerated(EnumType.STRING)
	private IndicatieGeheim indicatieGeheim;

	@Enumerated(EnumType.STRING)
	@Deprecated(forRemoval = true)
	private AanduidingBijzonderNederlanderschap aanduidingBijzonderNederlanderschap;

	@Temporal(TemporalType.DATE)
	private Date datumAangaanPartnerschap;

	@Temporal(TemporalType.DATE)
	private Date datumOntbindingPartnerschap;

	@Deprecated(forRemoval = true)
	private String redenOntbindingPartnerschap;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private Gemeente registerGemeenteAkteOverlijden;

	private String akteNummerOverlijden;

	@Temporal(TemporalType.DATE)
	private Date datumAanvangAdreshouding;

	@Temporal(TemporalType.DATE)
	private Date datumVestigingNederland;

	@Enumerated(EnumType.STRING)
	private Aanhef aanhef;

	@Column(length = 200)
	private String achternaam;

	@ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinTable(schema = "algemeen", name = "pat_persoon_adressen")
	private List<Adres> adressen = new ArrayList<>();

	@Column(length = 12)
	private String bsn;

	@Deprecated(forRemoval = true)
	private Date bsnControleDatum;

	@Deprecated(forRemoval = true)
	private Boolean bsnGeverifieerd;

	@ManyToOne(fetch = FetchType.LAZY)
	@Deprecated(forRemoval = true)
	private OrganisatieMedewerker bsnOrganisatieMedewerker;

	@Column(length = MAX_EMAIL_LENGTH)
	private String emailadres;

	@Column
	@Temporal(TemporalType.DATE)
	private Date geboortedatum;

	@Enumerated(EnumType.STRING)
	@Deprecated(forRemoval = true)
	private nl.topicuszorg.gba.model.Land geboorteland;

	@Column(nullable = true)
	@Deprecated(forRemoval = true)
	private String geboorteplaats;

	@Enumerated(EnumType.STRING)
	private Geslacht geslacht;

	@Column(length = 20)
	private String mobielnummer;

	@Enumerated(EnumType.STRING)
	private NaamGebruik naamGebruik;

	@AuditJoinTable(name = "pat_persoon_nats_AUD")
	@ElementCollection
	@CollectionTable(schema = "algemeen", name = "pat_persoon_nationaliteiten", joinColumns = @JoinColumn(name = "pat_persoon_id"))
	@Enumerated(EnumType.STRING)
	private List<nl.topicuszorg.gba.model.Nationaliteit> nationaliteiten = new ArrayList<>();

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date overlijdensdatum;

	@Column(length = 200)
	private String partnerAchternaam;

	@Column(length = 10)
	private String partnerTussenvoegsel;

	@OneToOne(cascade = CascadeType.ALL, optional = false)
	private Client patient;

	@Deprecated(forRemoval = true)
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY, mappedBy = "persoon")
	private List<Polis> polissen = new ArrayList<>();

	@Deprecated(forRemoval = true)
	@Column(length = 100)
	private String roepnaam;

	@Column(length = 20)
	private String telefoonnummer1;

	@Column(length = 20)
	private String telefoonnummer2;

	@Deprecated(forRemoval = true)
	@Column(length = 20)
	private String faxnummer;

	@Column(length = 10)
	private String tussenvoegsel;

	@Deprecated(forRemoval = true)
	@Column(length = 28)
	private String voorletters;

	@Column(length = 255)
	private String voornaam;

	@Deprecated(forRemoval = true)
	private Boolean meerling;

	@Deprecated(forRemoval = true)
	@Enumerated(EnumType.STRING)
	private DocumentType documentType;

	@Deprecated(forRemoval = true)
	@Column(length = 20)
	private String widnummer;

	@Deprecated(forRemoval = true)
	private Date widControleDatum;

	@Deprecated(forRemoval = true)
	private Boolean widGecontroleerd;

	@Deprecated(forRemoval = true)
	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker widOrganisatieMedewerker;

	@Deprecated(forRemoval = true)
	private Date widRegistreerDatum;

	@Deprecated(forRemoval = true)
	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker widGeregistreerdDoor;

	@Deprecated(forRemoval = true)
	@Column(length = 20)
	private String fonetischAchternaam;

	@Deprecated(forRemoval = true)
	@Column(length = 20)
	private String fonetischPartnerAchternaam;

	@Deprecated(forRemoval = true)
	@Column(length = 20)
	private String fonetischRoepnaam;

	@Deprecated(forRemoval = true)
	@Column(length = 20)
	private String fonetischVoornaam;

	private String titel;

	@Deprecated(forRemoval = true)
	@Enumerated(EnumType.STRING)
	private BurgelijkeStaat burgelijkeStaat;

	public Date getGeboortedatum()
	{
		if (DateUtil.compareEquals(BUGDATUM, geboortedatum))
		{
			var localDateTime = DateUtil.toLocalDateTime(geboortedatum);
			geboortedatum = DateUtil.toUtilDate(localDateTime.plusSeconds(28));
		}
		return geboortedatum;
	}

	@Deprecated(forRemoval = true)
	public Adres getAdres()
	{
		Adres adres = null;
		if (adressen != null && !adressen.isEmpty())
		{
			for (Adres a : adressen)
			{
				if (!a.getAanschrijfAdres())
				{
					adres = a;
				}
			}
		}
		return adres;
	}
}
