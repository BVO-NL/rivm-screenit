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
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Where;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen")
@Audited
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
@Getter
@Setter
public class Rol extends AbstractHibernateObject implements INaam, IActief, IBevolkingsonderzoek
{
	private String naam;

	private String description;

	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "rol")
	@Where(clause = "actief = true")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Permissie> permissies = new ArrayList<>();

	@ManyToOne(fetch = FetchType.LAZY)
	private Rol parentRol;

	@ElementCollection(targetClass = Bevolkingsonderzoek.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	@CollectionTable(schema = "algemeen", name = "rol_bevolkingsonderzoeken")
	@NotAudited
	private List<Bevolkingsonderzoek> bevolkingsonderzoeken = new ArrayList<>();

	private Boolean actief = true;

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
	public String toString()
	{
		return "Rol [naam=" + naam + "]";
	}
}
