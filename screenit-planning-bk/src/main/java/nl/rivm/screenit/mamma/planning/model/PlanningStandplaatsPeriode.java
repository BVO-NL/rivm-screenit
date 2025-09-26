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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Getter
@Setter
public class PlanningStandplaatsPeriode extends PlanningConceptEntiteit
{
	@Getter(AccessLevel.NONE)
	private final Lock lock = new ReentrantLock();

	private final Integer standplaatsRondeVolgNr;

	private final Set<PlanningBlokkade> blokkadeNavigableSet = new HashSet<>();

	@Setter
	private PlanningStandplaatsRonde standplaatsRonde;

	@Setter
	private PlanningScreeningsEenheid screeningsEenheid;

	@Setter
	private Integer screeningsEenheidVolgNr;

	@Setter
	private LocalDate vanaf;

	@Setter
	private LocalDate totEnMet;

	@Setter
	private Boolean prognose;

	@Getter
	private BigDecimal somGewogenDatum = BigDecimal.ZERO;

	@Getter
	private BigDecimal beschikbaarTotaal = BigDecimal.ZERO;

	@Getter
	private BigDecimal beschikbaarVoorJaarovergangTotaal = BigDecimal.ZERO;

	public PlanningStandplaatsPeriode(Long id, Integer screeningsEenheidVolgNr, Integer standplaatsRondeVolgNr, LocalDate vanaf, Boolean prognose, LocalDate totEnMet)
	{
		super(id);
		this.screeningsEenheidVolgNr = screeningsEenheidVolgNr;
		this.standplaatsRondeVolgNr = standplaatsRondeVolgNr;
		this.vanaf = vanaf;
		this.prognose = prognose;
		this.totEnMet = totEnMet;
	}

	public void lock()
	{
		LOG.trace("Lock SP: {}", getId());
		lock.lock();
	}

	public void unlock()
	{
		LOG.trace("Unlock SP: {}", getId());
		lock.unlock();
	}

	public void await()
	{
		LOG.trace("Await SP: {}", getId());
		lock.lock();
		lock.unlock();
	}

	public boolean gesplitst()
	{
		return getStandplaatsRonde().getStandplaatsPeriodeNavigableSet().higher(this) != null;
	}

	public void clear()
	{
		this.somGewogenDatum = BigDecimal.ZERO;
		this.beschikbaarTotaal = BigDecimal.ZERO;
		this.beschikbaarVoorJaarovergangTotaal = BigDecimal.ZERO;
	}

	public void add(long epochDay, BigDecimal beschikbaarTotaal, boolean voorJaarovergang)
	{
		this.somGewogenDatum = this.somGewogenDatum.add(new BigDecimal(epochDay).multiply(beschikbaarTotaal));
		this.beschikbaarTotaal = this.beschikbaarTotaal.add(beschikbaarTotaal);

		if (voorJaarovergang)
		{
			this.beschikbaarVoorJaarovergangTotaal = this.beschikbaarVoorJaarovergangTotaal.add(beschikbaarTotaal);
		}
	}
}
