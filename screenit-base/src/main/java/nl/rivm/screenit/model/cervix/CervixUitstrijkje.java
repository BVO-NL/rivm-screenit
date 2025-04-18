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

import java.io.Serial;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToOne;

import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;

import org.hibernate.envers.Audited;

@Entity
@Audited
public class CervixUitstrijkje extends CervixMonster
{
	@Serial
	private static final long serialVersionUID = 1L;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixLabformulier labformulier;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixCytologieOrder cytologieOrder;

	@OneToOne(fetch = FetchType.EAGER)
	private CervixCytologieVerslag cytologieVerslag;

	@Enumerated(EnumType.STRING)
	private CervixUitstrijkjeStatus uitstrijkjeStatus;

	@OneToOne(fetch = FetchType.LAZY)
	private CervixHuisartsBericht huisartsBericht;

	@Column(nullable = true, length = 3)
	private String controleLetters;

	public CervixLabformulier getLabformulier()
	{
		return labformulier;
	}

	public void setLabformulier(CervixLabformulier labformulier)
	{
		this.labformulier = labformulier;
	}

	public CervixCytologieVerslag getCytologieVerslag()
	{
		return cytologieVerslag;
	}

	public void setCytologieVerslag(CervixCytologieVerslag verslag)
	{
		this.cytologieVerslag = verslag;
	}

	public CervixUitstrijkjeStatus getUitstrijkjeStatus()
	{
		return uitstrijkjeStatus;
	}

	public void setUitstrijkjeStatus(CervixUitstrijkjeStatus uitstrijkjeStatus)
	{
		this.uitstrijkjeStatus = uitstrijkjeStatus;
	}

	public CervixCytologieOrder getCytologieOrder()
	{
		return cytologieOrder;
	}

	public void setCytologieOrder(CervixCytologieOrder cytologieOrder)
	{
		this.cytologieOrder = cytologieOrder;
	}

	public CervixHuisartsBericht getHuisartsBericht()
	{
		return huisartsBericht;
	}

	public void setHuisartsBericht(CervixHuisartsBericht huisartsBericht)
	{
		this.huisartsBericht = huisartsBericht;
	}

	public String getControleLetters()
	{
		return controleLetters;
	}

	public void setControleLetters(String controleLetters)
	{
		this.controleLetters = controleLetters;
	}

}
