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

import java.util.Date;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.enums.DatumPrecisie;
import nl.rivm.screenit.model.enums.IndicatieGeheim;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@NoArgsConstructor
@Entity(name = "persoon")
@Table(schema = "gedeeld",
	indexes = { @Index(name = "persoon_geboortedatumIndex", columnList = "geboortedatum") },
	uniqueConstraints = { @UniqueConstraint(columnNames = "bsn"), @UniqueConstraint(columnNames = "anummer") })
@Audited
public class Persoon extends AbstractHibernateObject
{

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

	@Temporal(TemporalType.DATE)
	private Date datumVertrokkenUitNederland;

	@Enumerated(EnumType.STRING)
	private DatumPrecisie geboortedatumPrecisie = DatumPrecisie.VOLLEDIG;

	private String titelCode;

	@Enumerated(EnumType.STRING)
	private IndicatieGeheim indicatieGeheim;

	@Temporal(TemporalType.DATE)
	private Date datumAangaanPartnerschap;

	@Temporal(TemporalType.DATE)
	private Date datumOntbindingPartnerschap;

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

	@Column(length = 12)
	private String bsn;

	@Column(length = MAX_EMAIL_LENGTH)
	private String emailadres;

	@Column
	@Temporal(TemporalType.DATE)
	private Date geboortedatum;

	@Enumerated(EnumType.STRING)
	private Geslacht geslacht;

	@Column(length = 20)
	private String mobielnummer;

	@Enumerated(EnumType.STRING)
	private NaamGebruik naamGebruik;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date overlijdensdatum;

	@Column(length = 200)
	private String partnerAchternaam;

	@Column(length = 10)
	private String partnerTussenvoegsel;

	@OneToOne(cascade = CascadeType.ALL, optional = false)
	private Client client;

	@Column(length = 20)
	private String telefoonnummer1;

	@Column(length = 20)
	private String telefoonnummer2;

	@Column(length = 10)
	private String tussenvoegsel;

	@Column(length = 255)
	private String voornaam;

	private String titel;

	public Date getGeboortedatum()
	{
		if (DateUtil.compareEquals(BUGDATUM, geboortedatum))
		{
			var localDateTime = DateUtil.toLocalDateTime(geboortedatum);
			geboortedatum = DateUtil.toUtilDate(localDateTime.plusSeconds(28));
		}
		return geboortedatum;
	}
}
