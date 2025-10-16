package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.util.DateUtil;

@Getter
@Setter
public class PlanningBlok extends PlanningConceptEntiteit
{
	private LocalTime vanaf;

	private LocalTime tot;

	private int aantalOnderzoeken;

	@Setter(AccessLevel.NONE)
	private final PlanningBeschikbaar beschikbaar = new PlanningBeschikbaar();

	private MammaCapaciteitBlokType capaciteitBlokType;

	private PlanningScreeningsEenheid screeningsEenheid;

	private PlanningDag dag;

	private String opmerkingen;

	private boolean minderValideAfspraakMogelijk;

	@Setter(AccessLevel.NONE)
	private List<PlanningMinderValideReservering> mindervalideReserveringen = new ArrayList<>();

	public PlanningBlok(Long id, LocalTime vanaf, LocalTime tot, int aantalOnderzoeken, MammaCapaciteitBlokType capaciteitBlokType, String opmerkingen,
		boolean minderValideAfspraakMogelijk, List<PlanningMinderValideReservering> mindervalideReserveringen)
	{
		super(id);
		this.vanaf = vanaf;
		this.tot = tot;
		this.aantalOnderzoeken = aantalOnderzoeken;
		this.capaciteitBlokType = capaciteitBlokType;
		this.opmerkingen = opmerkingen;
		this.minderValideAfspraakMogelijk = minderValideAfspraakMogelijk;
		this.mindervalideReserveringen.addAll(mindervalideReserveringen);
	}

	public Date getDateVanaf()
	{
		return DateUtil.toUtilDate(vanaf, dag.getDatum());
	}

	public Date getDateTot()
	{
		return DateUtil.toUtilDate(tot, dag.getDatum());
	}

}
