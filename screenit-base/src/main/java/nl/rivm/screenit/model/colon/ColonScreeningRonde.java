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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
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
import nl.rivm.screenit.model.colon.enums.ColonDefinitiefVervolgbeleid;

import org.hibernate.annotations.Cascade;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(schema = "colon", name = "screening_ronde", indexes = { @Index(name = "idx_colon_screening_ronde_status", columnList = "status"),
	@Index(name = "idx_colon_screening_ronde_definitiefvervolgbeleid", columnList = "definitiefvervolgbeleid") })
@Audited
@Getter
@Setter
public class ColonScreeningRonde extends ScreeningRonde<ColonDossier, ColonBrief, ColonAfmelding, ColonUitnodiging>
{
	@OneToMany(mappedBy = "screeningRonde", cascade = CascadeType.ALL)
	private List<ColonFitRegistratie> fitRegistraties = new ArrayList<>();

	@OneToOne(cascade = CascadeType.ALL)
	private ColonFitRegistratie laatsteFitRegistratie;

	@Deprecated
	@OneToOne(cascade = CascadeType.ALL)
	private ColonFitRegistratie laatsteExtraFitRegistratie;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Audited(targetAuditMode = NOT_AUDITED)
	@Cascade(org.hibernate.annotations.CascadeType.SAVE_UPDATE)
	private EnovationHuisarts huisarts;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVastleggenHuisarts;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	private List<ColonIntakeAfspraak> afspraken = new ArrayList<>();

	@ManyToOne(cascade = CascadeType.ALL)
	private ColonIntakeAfspraak laatsteAfspraak;

	@OneToMany(mappedBy = "screeningRonde", cascade = CascadeType.ALL)
	private List<ColonVerslag> verslagen = new ArrayList<>();

	@ManyToOne(optional = false)
	private ColonDossier dossier;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	private List<ColonUitnodiging> uitnodigingen = new ArrayList<>();

	@ManyToOne()
	private ColonUitnodiging laatsteUitnodiging;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	private List<ColonBrief> brieven = new ArrayList<>();

	@ManyToOne()
	private ColonBrief laatsteBrief;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	private List<ColonAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(cascade = CascadeType.ALL)
	private ColonAfmelding laatsteAfmelding;

	@OneToOne(cascade = CascadeType.ALL, mappedBy = "ronde")
	private OpenUitnodiging openUitnodiging;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "screeningsRonde")
	@NotAudited
	private List<ColonHuisartsBericht> huisartsBerichten = new ArrayList<>();

	@Deprecated
	@Enumerated(EnumType.STRING)
	private ColonDefinitiefVervolgbeleid definitiefVervolgbeleid;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "screeningsRonde")
	@NotAudited
	private List<ColonKoppelresultaatKankerregistratie> koppelresultatenKankerregistratie = new ArrayList<>();

	private boolean gepusht;
}
