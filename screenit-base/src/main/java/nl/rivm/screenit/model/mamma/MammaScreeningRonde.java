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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
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

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaFollowUpConclusieStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.util.SkipFieldForDiff;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(
	schema = "mamma",
	name = "screening_ronde",
	indexes = {
		@Index(name = "idx_MAMMA_SCREENING_RONDE_STATUS", columnList = "status"),
		@Index(name = "idx_MAMMA_SCREENING_RONDE_CREATIE_DATUM", columnList = "creatieDatum") }
)
@Audited
@Getter
@Setter
public class MammaScreeningRonde extends ScreeningRonde<MammaDossier, MammaBrief, MammaAfmelding, MammaUitnodiging>
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaDossier dossier;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<MammaUitnodiging> uitnodigingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private MammaUitnodiging laatsteUitnodiging;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<MammaUitstel> uitstellen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaUitstel laatsteUitstel;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<MammaBrief> brieven = new ArrayList<>();

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	private List<MammaDigitaalClientBericht> berichten = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private MammaBrief laatsteBrief;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<MammaAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaAfmelding laatsteAfmelding;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaStandplaatsRonde standplaatsRonde;

	@Column(unique = true, nullable = false)
	private Long uitnodigingsNr;

	@ManyToOne(fetch = FetchType.LAZY, optional = true, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Audited(targetAuditMode = NOT_AUDITED)
	@Cascade(CascadeType.SAVE_UPDATE)
	private EnovationHuisarts huisarts;

	@Column
	@Enumerated(EnumType.STRING)
	private MammaGeenHuisartsOption geenHuisartsOptie;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVastleggenHuisarts;

	@Column(nullable = false)
	private boolean isGeforceerd;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	private MammaKansberekeningScreeningRondeEvent screeningRondeEvent;

	@Column(nullable = true, length = 6)
	@SkipFieldForDiff
	private String postcode;

	@Column(nullable = false)
	private Boolean minderValideOnderzoekZiekenhuis;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaFollowUpConclusieStatus followUpConclusieStatus;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date followUpConclusieStatusGewijzigdOp;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<MammaFollowUpVerslag> followUpVerslagen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde")
	private List<MammaFollowUpRadiologieVerslag> followUpRadiologieVerslagen = new ArrayList<>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde")
	private List<MammaUploadBeeldenVerzoek> uploadBeeldenVerzoeken = new ArrayList<>();

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = true)
	private Date laatstGebeldFollowUpNietGedownload;

	@OneToOne(optional = true, fetch = FetchType.LAZY)
	private MammaOnderzoek laatsteOnderzoek;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde", cascade = { jakarta.persistence.CascadeType.REMOVE })
	@Cascade(CascadeType.DELETE)
	private List<MammaConclusieReview> conclusieReviews = new ArrayList<>();

}
