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

import lombok.Getter;

@Getter
public final class PlanningBenodigdJaar
{
	private final Integer jaar;

	private BigDecimal totaal;

	private BigDecimal nieuw;

	private BigDecimal oud;

	private BigDecimal eersteOnderzoekCorrectie;

	public PlanningBenodigdJaar(Integer jaar)
	{
		clear();
		this.jaar = jaar;
	}

	public void add(BigDecimal benodigd)
	{
		totaal = totaal.add(benodigd);
	}

	public void addToNieuw(BigDecimal benodigd)
	{
		nieuw = nieuw.add(benodigd);
	}

	public void addToOud(BigDecimal benodigd)
	{
		oud = oud.add(benodigd);
	}

	public void addToEersteOnderzoekCorrectie(BigDecimal benodigd)
	{
		eersteOnderzoekCorrectie = eersteOnderzoekCorrectie.add(benodigd);
	}

	public void add(PlanningBenodigdJaar benodigdJaar)
	{
		totaal = totaal.add(benodigdJaar.totaal);
		nieuw = nieuw.add(benodigdJaar.nieuw);
		oud = oud.add(benodigdJaar.oud);
		eersteOnderzoekCorrectie = eersteOnderzoekCorrectie.add(benodigdJaar.getEersteOnderzoekCorrectie());
	}

	public void subtract(PlanningBenodigdJaar benodigdJaar)
	{
		totaal = totaal.subtract(benodigdJaar.totaal);
		nieuw = nieuw.subtract(benodigdJaar.nieuw);
		oud = oud.subtract(benodigdJaar.oud);
		eersteOnderzoekCorrectie = eersteOnderzoekCorrectie.subtract(benodigdJaar.getEersteOnderzoekCorrectie());
	}

	public void clear()
	{
		totaal = BigDecimal.ZERO;
		nieuw = BigDecimal.ZERO;
		oud = BigDecimal.ZERO;
		eersteOnderzoekCorrectie = BigDecimal.ZERO;
	}

}
