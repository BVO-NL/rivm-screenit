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
import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenOrganisatieOvereenkomst;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.Hibernate;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity
@Table(schema = "algemeen", name = "organisatie", indexes = { @Index(name = "idx_organisatie_agbcode", columnList = "agbcode"),
	@Index(name = "idx_organisatie_actief", columnList = "actief") })
@Audited
public class Organisatie extends AbstractHibernateObject implements IActief
{
	@Column(nullable = false, length = HibernateMagicNumber.L100)
	private String naam;

	@Column(unique = true)
	private String agbcode;

	@Column(unique = true)
	private String uziAbonneenummer;

	@Column(length = 20)
	private String applicatieId;

	@Column(length = 25)
	private String telefoon;

	@Column(length = 20)
	private String fax;

	@Column(length = 100)
	private String email;

	@Column(length = 100)
	private String communicatieAdres;

	@OneToMany(mappedBy = "organisatie", orphanRemoval = true, cascade = jakarta.persistence.CascadeType.ALL)
	private List<OrganisatieMedewerker> organisatieMedewerkers = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private Adres adres;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private Adres postbusAdres;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE })
	@Cascade(CascadeType.SAVE_UPDATE)
	private Adres antwoordnummerAdres;

	@Column(nullable = false)
	private Boolean actief = Boolean.TRUE;

	@Column(length = HibernateMagicNumber.L20)
	private String telefoon2;

	@Column(length = HibernateMagicNumber.L200)
	private String website;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private OrganisatieType organisatieType;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	private Organisatie parent;

	@NotAudited
	@ManyToOne(fetch = FetchType.LAZY)
	private Organisatie regio;

	@NotAudited
	@OneToMany(mappedBy = "parent", fetch = FetchType.LAZY)
	private List<Organisatie> children = new ArrayList<>();

	@NotAudited
	@Column(length = HibernateMagicNumber.L512, unique = true)
	private String rootOid;

	@ManyToOne
	@NotAudited
	private Medewerker gemachtigde;

	@OneToMany(fetch = FetchType.LAZY)
	@JoinTable(schema = "gedeeld", name = "organisatie_documents", joinColumns = { @JoinColumn(name = "organisatie") })
	private List<UploadDocument> documents;

	@ManyToOne
	@NotAudited
	private Medewerker contactPersoon;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "organisatie")
	@NotAudited
	private List<AfgeslotenOrganisatieOvereenkomst> afgeslotenOvereenkomsten;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date mammaRadiologieGebeld;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "organisatie")
	@NotAudited
	private List<OrganisatieParameter> parameters = new ArrayList<>();

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		if (getId() != null)
		{
			result = prime * result + getId().hashCode();
		}
		else
		{
			result = prime * result + (actief == null ? 0 : actief.hashCode());
			result = prime * result + (getNaam() == null ? 0 : getNaam().hashCode());
			result = prime * result + (getAgbcode() == null ? 0 : getAgbcode().hashCode());
		}
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		boolean returnValue = true;
		if (obj == null)
		{
			returnValue = false;
		}
		else if (Hibernate.getClass(this) != Hibernate.getClass(obj))
		{
			returnValue = false;
		}
		else
		{
			Organisatie other = (Organisatie) obj;
			if (getId() == null)
			{
				if (other.getId() != null)
				{
					returnValue = false;
				}
			}
			else if (!getId().equals(other.getId()))
			{
				returnValue = false;
			}

			if (returnValue && getId() == null)
			{
				if (actief == null)
				{
					if (other.actief != null)
					{
						returnValue = false;
					}
				}
				else if (!actief.equals(other.actief))
				{
					returnValue = false;
				}
				if (getNaam() == null)
				{
					if (other.getNaam() != null)
					{
						returnValue = false;
					}
				}
				else if (!getNaam().equals(other.getNaam()))
				{
					returnValue = false;
				}
				if (getAgbcode() == null)
				{
					if (other.getAgbcode() != null)
					{
						returnValue = false;
					}
				}
				else if (!getAgbcode().equals(other.getAgbcode()))
				{
					returnValue = false;
				}
			}
		}

		return returnValue;
	}
}
