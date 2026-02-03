package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "standplaats_ronde")
@Audited
@Getter
@Setter
public class MammaStandplaatsRonde extends AbstractHibernateObject
{
	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private MammaStandplaats standplaats;

	@Column()
	private Integer afspraakDrempel;

	@OneToMany(mappedBy = "standplaatsRonde", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaStandplaatsPeriode> standplaatsPerioden = new ArrayList<>();

	@Column(precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S5)
	private BigDecimal interval;

	@ManyToOne(fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats achtervangStandplaats;

	@ManyToMany(fetch = FetchType.LAZY)
	@JoinTable(
		schema = "mamma",
		name = "standplaats_ronde_afspraakcapaciteit_beschikbaar_voor",
		joinColumns = { @JoinColumn(name = "standplaats_ronde") },
		inverseJoinColumns = { @JoinColumn(name = "screeningorganisatie") })
	@DiffSpecs(displayProperty = "naam", listSupported = true)
	private List<ScreeningOrganisatie> afspraakcapaciteitBeschikbaarVoor = new ArrayList<>();

	@OneToOne(mappedBy = "standplaatsRonde")
	@SkipFieldForDiff
	private MammaKansberekeningStandplaatsRondeGemiddelden standplaatsRondeGemiddelden;

	@Column(nullable = false)
	private Boolean achtervangToegepast;

	@Temporal(TemporalType.DATE)
	private Date mindervalideUitnodigenVanaf;

	@Column(nullable = false, precision = HibernateMagicNumber.P9, scale = HibernateMagicNumber.S2)
	private BigDecimal extraMindervalideCapaciteitUitgenodigd = BigDecimal.ZERO;
}
