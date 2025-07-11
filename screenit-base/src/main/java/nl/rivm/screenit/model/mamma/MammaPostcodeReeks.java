package nl.rivm.screenit.model.mamma;

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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "postcode_reeks", indexes = @Index(name = "idx_mamma_postcode_reeks_vanaf_tot", columnList = "vanPostcode, totPostcode"))
@Audited
@Getter
@Setter
public class MammaPostcodeReeks extends AbstractHibernateObject
{
	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats standplaats;

	@Column(nullable = false, length = HibernateMagicNumber.L6)
	private String vanPostcode;

	@Column(nullable = false, length = HibernateMagicNumber.L6)
	private String totPostcode;

	public void setVanPostcode(String vanPostcode)
	{
		if (vanPostcode != null)
		{
			this.vanPostcode = vanPostcode.toUpperCase().replace(" ", "");
		}
		else
		{
			this.vanPostcode = null;
		}
	}

	public void setTotPostcode(String totPostcode)
	{
		if (totPostcode != null)
		{
			this.totPostcode = totPostcode.toUpperCase().replace(" ", "");
		}
		else
		{
			this.totPostcode = null;
		}
	}
}
