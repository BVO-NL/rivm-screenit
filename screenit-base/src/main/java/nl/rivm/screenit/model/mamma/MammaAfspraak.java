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

import java.math.BigDecimal;
import java.util.Date;

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
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaIdentificatiesoort;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.FetchProfile;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(
	schema = "mamma",
	name = "afspraak",
	indexes = {
		@Index(name = "idx_mamma_afspraak_vanaf", columnList = "vanaf"),
		@Index(name = "idx_mamma_afspraak_status", columnList = "status")
	})
@Audited
@FetchProfile(
	name = "kansberekening",
	fetchOverrides = {
		@FetchProfile.FetchOverride(entity = MammaAfspraak.class, association = "onderzoek", mode = FetchMode.JOIN)
	})
@Getter
@Setter
public class MammaAfspraak extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	@SkipFieldForDiff
	private Date creatiedatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	@SkipFieldForDiff
	private MammaAfspraakStatus status;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	@SkipFieldForDiff
	private Date ingeschrevenOp;

	@ManyToOne(fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private InstellingGebruiker ingeschrevenDoor;

	@Column
	@Enumerated(EnumType.STRING)
	@SkipFieldForDiff
	private MammaVerzettenReden verzettenReden;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaUitnodiging uitnodiging;

	@Column(nullable = false)
	private Boolean bezwaarAangevraagd;

	@ManyToOne(fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaCapaciteitBlok capaciteitBlok;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "standplaatsRonde.standplaats.naam")
	private MammaStandplaatsPeriode standplaatsPeriode;

	@OneToOne(fetch = FetchType.LAZY)
	private MammaOnderzoek onderzoek;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date vanaf;

	@OneToOne(mappedBy = "afspraak", optional = false, fetch = FetchType.LAZY)
	@NotAudited
	@SkipFieldForDiff
	private MammaOpkomstkans opkomstkans;

	@Column
	@Enumerated(EnumType.STRING)
	@SkipFieldForDiff
	private MammaIdentificatiesoort identificatiesoort;

	@Column
	@SkipFieldForDiff
	private String identificatienummer;

	@Transient
	@SkipFieldForDiff
	private BigDecimal benodigdeCapaciteit;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	@SkipFieldForDiff
	private Date afgezegdOp;

	@OneToOne(fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	@SkipFieldForDiff
	private MammaKansberekeningAfspraakEvent afspraakEvent;

	@Column(length = 6)
	@SkipFieldForDiff
	private String postcode;

	@Column(nullable = false)
	private boolean geforceerdeAfspraak = false;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private SmsStatus smsStatus = SmsStatus.GEEN;
}
