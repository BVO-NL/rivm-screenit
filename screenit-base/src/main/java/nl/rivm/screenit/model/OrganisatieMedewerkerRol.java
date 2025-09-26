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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "algemeen")
@Audited
@Getter
@Setter
public class OrganisatieMedewerkerRol extends AbstractHibernateObject implements IActief, IBevolkingsonderzoek
{
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(nullable = false)
	private OrganisatieMedewerker organisatieMedewerker;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(nullable = false)
	private Rol rol;

	@ElementCollection(targetClass = Bevolkingsonderzoek.class, fetch = FetchType.EAGER)
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	@CollectionTable(schema = "algemeen", name = "organisatie_medewerker_rol_bevolkingsonderzoeken")
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken = new ArrayList<>();

	private Date beginDatum;

	private Date eindDatum;

	@Column(nullable = false)
	private Boolean actief = true;

	@Deprecated
	@Transient
	public boolean isRolActief()
	{
		Boolean isActief = actief;

		Calendar cal = Calendar.getInstance();
		cal.add(Calendar.DATE, -1);

		Date nu = new Date();
		if (beginDatum != null && nu.before(beginDatum))
		{
			isActief = false;
		}
		if (eindDatum != null && cal.getTime().after(eindDatum))
		{
			isActief = false;
		}

		if (rol != null && Boolean.FALSE.equals(rol.getActief()))
		{
			isActief = false;
		}

		return !Boolean.FALSE.equals(isActief);
	}

	@Override
	public Boolean getExactMatch()
	{

		return null;
	}

	@Override
	public void setExctMatch(Boolean exactMatch)
	{

	}

	@Override
	protected boolean concreateEquals(AbstractHibernateObject obj)
	{
		OrganisatieMedewerkerRol other = (OrganisatieMedewerkerRol) obj;
		if (getRol() == null)
		{
			if (other.getRol() != null)
			{
				return false;
			}
		}
		else if (!getRol().equals(other.getRol()))
		{
			return false;
		}
		if (getEindDatum() == null)
		{
			if (other.getEindDatum() != null)
			{
				return false;
			}
		}
		else if (!getEindDatum().equals(other.getEindDatum()))
		{
			return false;
		}
		if (getBeginDatum() == null)
		{
			if (other.getBeginDatum() != null)
			{
				return false;
			}
		}
		else if (!getBeginDatum().equals(other.getBeginDatum()))
		{
			return false;
		}
		return super.concreateEquals(obj);
	}
}
