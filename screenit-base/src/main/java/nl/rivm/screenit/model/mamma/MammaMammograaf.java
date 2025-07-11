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

import java.util.Objects;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.apache.commons.lang3.ObjectUtils;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "mammograaf",
	uniqueConstraints = { @UniqueConstraint(name = "uc_mammograaf_aeTitle", columnNames = { "aeTitle" }),
		@UniqueConstraint(name = "uc_mammograaf_werkstationIpAdres", columnNames = { "werkstationIpAdres" }) })
@Audited
public class MammaMammograaf extends AbstractHibernateObject
{

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningsEenheid screeningsEenheid;

	@Column(nullable = false)
	private String aeTitle;

	@Column(nullable = false)
	private String werkstationIpAdres;

	public MammaScreeningsEenheid getScreeningsEenheid()
	{
		return screeningsEenheid;
	}

	public MammaMammograaf setScreeningsEenheid(MammaScreeningsEenheid screeningsEenheid)
	{
		this.screeningsEenheid = screeningsEenheid;
		return this;
	}

	public String getAeTitle()
	{
		return aeTitle;
	}

	public MammaMammograaf setAeTitle(String aeTitle)
	{
		this.aeTitle = aeTitle;
		return this;
	}

	public String getWerkstationIpAdres()
	{
		return werkstationIpAdres;
	}

	public MammaMammograaf setWerkstationIpAdres(String werkstationIpAdres)
	{
		this.werkstationIpAdres = werkstationIpAdres;
		return this;
	}

	@Override
	protected boolean concreateEquals(AbstractHibernateObject obj)
	{
		return ObjectUtils.equals(((MammaMammograaf) obj).aeTitle, aeTitle)
			&& ObjectUtils.equals(((MammaMammograaf) obj).werkstationIpAdres, werkstationIpAdres);
	}

	@Override
	protected int concreateHashCode(int prime, int result)
	{
		return Objects.hash(aeTitle, werkstationIpAdres);
	}

}
