
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

import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Audited
public class BagAdres extends Adres
{

	@Serial
	private static final long serialVersionUID = 1L;

	private String naamOpenbareRuimte;

	private String identificatieCodeVerblijfplaats;

	private String identificatieCodeNummerAanduiding;

	@ManyToOne(fetch = FetchType.LAZY)
	private Gemeente gbaGemeente;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private PostcodeCoordinaten postcodeCoordinaten;

	public String getNaamOpenbareRuimte()
	{
		return naamOpenbareRuimte;
	}

	public void setNaamOpenbareRuimte(String naamOpenbareRuimte)
	{
		this.naamOpenbareRuimte = naamOpenbareRuimte;
	}

	public String getIdentificatieCodeVerblijfplaats()
	{
		return identificatieCodeVerblijfplaats;
	}

	public void setIdentificatieCodeVerblijfplaats(String identificatieCodeVerblijfplaats)
	{
		this.identificatieCodeVerblijfplaats = identificatieCodeVerblijfplaats;
	}

	public String getIdentificatieCodeNummerAanduiding()
	{
		return identificatieCodeNummerAanduiding;
	}

	public void setIdentificatieCodeNummerAanduiding(String identificatieCodeNummerAanduiding)
	{
		this.identificatieCodeNummerAanduiding = identificatieCodeNummerAanduiding;
	}

	public Gemeente getGbaGemeente()
	{
		return gbaGemeente;
	}

	public void setGbaGemeente(Gemeente gbaGemeente)
	{
		this.gbaGemeente = gbaGemeente;
	}

	@Deprecated
	@Override
	public String getGemeente()
	{
		return super.getGemeente();
	}

	@Deprecated
	@Override
	public void setGemeente(String gemeente)
	{
		super.setGemeente(gemeente);
	}

	@Override
	@Deprecated
	public String getGemeenteCode()
	{
		return super.getGemeenteCode();
	}

	@Override
	@Deprecated
	public void setGemeenteCode(String gemeenteCode)
	{
		super.setGemeenteCode(gemeenteCode);
	}

	@Override
	@Deprecated
	public Boolean getGeheimadres()
	{
		return super.getGeheimadres();
	}

	@Override
	@Deprecated
	public void setGeheimadres(Boolean geheimadres)
	{
		super.setGeheimadres(geheimadres);
	}

	public PostcodeCoordinaten getPostcodeCoordinaten()
	{
		return postcodeCoordinaten;
	}

	public void setPostcodeCoordinaten(PostcodeCoordinaten postcodeCoordinaten)
	{
		this.postcodeCoordinaten = postcodeCoordinaten;
	}

}
