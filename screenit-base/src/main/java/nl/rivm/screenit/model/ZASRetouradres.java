
package nl.rivm.screenit.model;

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
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Audited
public class ZASRetouradres extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private ScreeningOrganisatie regio;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private BMHKLaboratorium laboratorium;

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private Adres adres;

	public ScreeningOrganisatie getRegio()
	{
		return regio;
	}

	public void setRegio(ScreeningOrganisatie regio)
	{
		this.regio = regio;
	}

	public BMHKLaboratorium getLaboratorium()
	{
		return laboratorium;
	}

	public void setLaboratorium(BMHKLaboratorium laboratorium)
	{
		this.laboratorium = laboratorium;
	}

	public Adres getAdres()
	{
		return adres;
	}

	public void setAdres(Adres adres)
	{
		this.adres = adres;
	}

}
