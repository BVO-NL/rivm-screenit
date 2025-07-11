package nl.rivm.screenit.model.colon;

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
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.RedenNietTeBeoordelen;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "IFOBTEST_STATUS", columnList = "status"), @Index(name = "idx_ifobttest_barcode", columnList = "barcode", unique = true) })
@Audited
@Getter
@Setter
public class IFOBTTest extends AbstractHibernateObject
{

	@Enumerated(EnumType.STRING)
	private IFOBTTestStatus status;

	@Enumerated(EnumType.STRING)
	private RedenNietTeBeoordelen redenNietTeBeoordelen;

	private boolean herinnering;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date verwerkingsDatum;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVerstuurd;

	@ManyToOne(optional = false)
	private ColonScreeningRonde colonScreeningRonde;

	@Column(nullable = false, unique = true)
	private String barcode;

	@OneToOne(mappedBy = "gekoppeldeTest", optional = true)
	private ColonUitnodiging colonUitnodiging;

	@OneToOne(mappedBy = "gekoppeldeExtraTest", optional = true)
	private ColonUitnodiging colonUitnodigingExtra;

	@Deprecated
	@Temporal(TemporalType.DATE)
	private Date afnameDatum;

	@Temporal(TemporalType.DATE)
	private Date analyseDatum;

	private BigDecimal uitslag;

	private BigDecimal normWaarde;

	private String flag;

	@Enumerated(EnumType.STRING)
	private ColonGeinterpreteerdeUitslag geinterpreteerdeUitslag;

	@ManyToOne
	@JoinColumn(name = "ifobtLaboratorium_id")
	private IFobtLaboratorium ifobtLaboratorium;

	private String instumentId;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private IFOBTType type;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument verwijderbrief;

	@Enumerated(EnumType.STRING)
	private PreferenceKey heraanmeldenTekstKey;

}
