package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeSet;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaMeldingNiveau;

@Getter
@Setter
public class PlanningStandplaatsRonde extends PlanningConceptEntiteit
{
	private PlanningStandplaats standplaats;

	private LocalDate gewogenGemiddeldeDatum;

	private BigDecimal beschikbaarTotaal;

	private Integer afspraakDrempel;

	@Getter
	private static final Comparator<PlanningStandplaatsPeriode> standplaatsPeriodeComparator = Comparator.comparing(PlanningStandplaatsPeriode::getStandplaatsRondeVolgNr);

	private final NavigableSet<PlanningStandplaatsPeriode> standplaatsPeriodeNavigableSet = new TreeSet<>(standplaatsPeriodeComparator);

	private MammaMeldingNiveau niveau = MammaMeldingNiveau.INFO;

	private final List<PlanningMelding> meldingList = new ArrayList<>();

	private BigDecimal interval; 

	private BigDecimal initieelInterval; 

	private PlanningStandplaats achtervangStandplaats;

	private List<PlanningScreeningsOrganisatie> afspraakcapaciteitBeschikbaarVoor = new ArrayList<>();

	private Boolean achtervangToegepast;

	private LocalDate mindervalideUitnodigenVanaf;

	private BigDecimal extraMindervalideCapaciteitUitgenodigd;

	private final Set<PlanningClient> screeningRondeTransportSet = new HashSet<>();

	public PlanningStandplaatsRonde(Long id, Integer afspraakDrempel, BigDecimal initieelInterval, PlanningStandplaats achtervangStandplaats,
		Boolean achtervangToegepast, LocalDate mindervalideUitnodigenVanaf, BigDecimal extraMindervalideCapaciteitUitgenodigd)
	{
		super(id);
		this.afspraakDrempel = afspraakDrempel;
		this.initieelInterval = initieelInterval;
		this.achtervangStandplaats = achtervangStandplaats;
		this.achtervangToegepast = achtervangToegepast;
		this.mindervalideUitnodigenVanaf = mindervalideUitnodigenVanaf;
		this.extraMindervalideCapaciteitUitgenodigd = extraMindervalideCapaciteitUitgenodigd;
	}

	public void clear()
	{
		gewogenGemiddeldeDatum = null;
		beschikbaarTotaal = BigDecimal.ZERO;
	}
}
