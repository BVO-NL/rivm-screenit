package nl.rivm.screenit.model.gba;

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

import jakarta.persistence.Entity;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(schema = "algemeen")
public class GbaVerwerkingEntry extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	private Long screeningOrganisatie;

	private Integer aantalNieuweBurgers = 0;

	private Integer aantalBijgewerkteBugers = 0;

	@ManyToOne
	@JsonIgnore
	private GbaVerwerkingsLog verwerkingsLog;

	public Long getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(Long screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

	public Integer getAantalNieuweBurgers()
	{
		return aantalNieuweBurgers;
	}

	public void setAantalNieuweBurgers(Integer aantalNieuweBurgers)
	{
		this.aantalNieuweBurgers = aantalNieuweBurgers;
	}

	public Integer getAantalBijgewerkteBugers()
	{
		return aantalBijgewerkteBugers;
	}

	public void setAantalBijgewerkteBugers(Integer aantalBijgewerkteBugers)
	{
		this.aantalBijgewerkteBugers = aantalBijgewerkteBugers;
	}

	public GbaVerwerkingsLog getVerwerkingsLog()
	{
		return verwerkingsLog;
	}

	public void setVerwerkingsLog(GbaVerwerkingsLog verwerkingsLog)
	{
		this.verwerkingsLog = verwerkingsLog;
	}
}
