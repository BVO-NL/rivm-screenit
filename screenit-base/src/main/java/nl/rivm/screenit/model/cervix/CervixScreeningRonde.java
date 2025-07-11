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

import java.time.LocalDate;
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

import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.verslag.CervixVerslag;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Table(
	schema = "cervix",
	name = "screening_ronde",
	indexes = { @Index(name = "idx_CERVIX_SCREENING_RONDE_STATUS", columnList = "status"),
		@Index(name = "idx_CERVIX_SCREENING_RONDE_LEEFTIJDCATEGORIE", columnList = "leeftijdcategorie"),
		@Index(name = "idx_CERVIX_SCREENING_RONDE_IN_VERVOLGONDERZOEK_DATUM", columnList = "inVervolgonderzoekDatum") })
@Audited
public class CervixScreeningRonde extends ScreeningRonde<CervixDossier, CervixBrief, CervixAfmelding, CervixUitnodiging>
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private CervixDossier dossier;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private CervixLeeftijdcategorie leeftijdcategorie;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<CervixUitnodiging> uitnodigingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging eersteUitnodiging;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging laatsteUitnodiging;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging uitnodigingVervolgonderzoek;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<CervixBrief> brieven = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixBrief laatsteBrief;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "screeningRonde")
	@NotAudited
	private List<CervixHuisartsBericht> huisartsBerichten = new ArrayList<>();

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<CervixAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private CervixAfmelding laatsteAfmelding;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitstel uitstel;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixMonster monsterHpvUitslag;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixUitstrijkje uitstrijkjeCytologieUitslag;

	@Temporal(TemporalType.TIMESTAMP)
	private Date inVervolgonderzoekDatum;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixUitstrijkje uitstrijkjeVervolgonderzoekUitslag;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixUitnodiging laatsteZasUitnodiging;

	@OneToMany(mappedBy = "screeningRonde", fetch = FetchType.LAZY)
	private List<CervixVerslag> verslagen = new ArrayList<>();

	@Column(nullable = true)
	private LocalDate controleUitstrijkjeDatum;

}
