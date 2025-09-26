package nl.rivm.screenit.model.cervix;

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
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.signaleringen.CervixMonsterSignalering;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity
@Table(
	schema = "cervix",
	name = "monster",
	indexes = {
		@Index(name = "idx_CERVIX_MONSTER_STATUS_DATUM", columnList = "statusDatum"),
		@Index(name = "idx_CERVIX_MONSTER_ONTVANGSTDATUM", columnList = "ontvangstdatum"),
		@Index(name = "idx_CERVIX_MONSTER_DATUM_ORU_VERSTUURD", columnList = "datumOruVerstuurd"),
		@Index(name = "idx_CERVIX_ZAS_ZAS_STATUS", columnList = "zasStatus") })
@Audited
public abstract class CervixMonster extends AbstractHibernateObject
{
	@OneToMany(mappedBy = "monster", fetch = FetchType.LAZY)
	private List<CervixVerrichting> verrichtingen = new ArrayList<>();

	@OneToOne(mappedBy = "monster", optional = false, fetch = FetchType.LAZY)
	private CervixUitnodiging uitnodiging;

	@Column(unique = true, nullable = false)
	private String monsterId;

	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangstdatum;

	@ManyToOne(optional = true, fetch = FetchType.EAGER)
	private CervixScreeningRonde ontvangstScreeningRonde;

	@ManyToOne(fetch = FetchType.LAZY)
	private BMHKLaboratorium laboratorium;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Enumerated(EnumType.STRING)
	private CervixNietAnalyseerbaarReden nietAnalyseerbaarReden;

	@ElementCollection(targetClass = CervixMonsterSignalering.class)
	@Column
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "cervix", name = "monster_signalering")
	@SkipFieldForDiff
	private List<CervixMonsterSignalering> signaleringen = new ArrayList<>();

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date datumOruVerstuurd;

	@NotAudited
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "monster")
	private List<CervixHpvBeoordeling> hpvBeoordelingen = new ArrayList<>();

	@NotAudited
	@OneToOne(fetch = FetchType.EAGER)
	private CervixHpvBeoordeling laatsteHpvBeoordeling;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixBrief brief;

	@Temporal(TemporalType.TIMESTAMP)
	private Date verwijderdDatum;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument verwijderdBrief;

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "cervix", name = "monster_barcode_afgedrukt")
	@NotAudited
	private List<Date> barcodeAfgedrukt = new ArrayList<>();

	@Column
	private String overigeSignalering;
}
