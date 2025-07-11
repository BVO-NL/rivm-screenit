package nl.rivm.screenit.model.logging;

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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "gedeeld")
@Inheritance(strategy = InheritanceType.JOINED)
public class LogEvent extends AbstractHibernateObject
{
	@Column(length = HibernateMagicNumber.L4096)
	private String melding;

	@Column(columnDefinition = "TEXT")
	private String volledigeMelding;

	@Enumerated(EnumType.STRING)
	private Level level = Level.INFO;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, mappedBy = "logEvent")
	private LogRegel logRegel;

	public LogRegel getLogRegel()
	{
		return logRegel;
	}

	public void setLogRegel(LogRegel logRegel)
	{
		this.logRegel = logRegel;
	}

	public LogEvent()
	{

	}

	public LogEvent(String melding, Level level)
	{
		this.melding = melding;
		this.level = level;
	}

	public LogEvent(String melding)
	{
		this.melding = melding;
	}

	public String getMelding()
	{
		return melding;
	}

	public void setMelding(String melding)
	{
		this.melding = melding;
	}

	public Level getLevel()
	{
		return level;
	}

	public void setLevel(Level level)
	{
		this.level = level;
	}

	public String getVolledigeMelding()
	{
		return volledigeMelding;
	}

	public void setVolledigeMelding(String volledigeMelding)
	{
		this.volledigeMelding = volledigeMelding;
	}
}
