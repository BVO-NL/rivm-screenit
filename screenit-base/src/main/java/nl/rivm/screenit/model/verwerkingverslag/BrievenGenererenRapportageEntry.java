package nl.rivm.screenit.model.verwerkingverslag;

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

import jakarta.persistence.Entity;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(schema = "gedeeld")
public class BrievenGenererenRapportageEntry extends AbstractHibernateObject
{
	@ManyToOne
	private BrievenGenererenRapportage rapportage;

	@ManyToOne
	private ScreeningOrganisatie screeningOrganisatie;

	private Integer aantalBrievenPerScreeningOrganisatie;

	public BrievenGenererenRapportage getRapportage()
	{
		return rapportage;
	}

	public void setRapportage(BrievenGenererenRapportage rapportage)
	{
		this.rapportage = rapportage;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{
		return screeningOrganisatie;
	}

	public void setScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		this.screeningOrganisatie = screeningOrganisatie;
	}

	public Integer getAantalBrievenPerScreeningOrganisatie()
	{
		return aantalBrievenPerScreeningOrganisatie;
	}

	public void setAantalBrievenPerScreeningOrganisatie(Integer aantalBrievenPerScreeningOrganisatie)
	{
		this.aantalBrievenPerScreeningOrganisatie = aantalBrievenPerScreeningOrganisatie;
	}
}
