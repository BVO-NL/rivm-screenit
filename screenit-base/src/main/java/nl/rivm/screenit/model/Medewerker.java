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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.nieuws.MedewerkerNieuwsItem;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.yubikey.model.YubiKey;

import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen", name = "medewerker",
	indexes = {
		@Index(name = "IDX_MEDEWERKER_ACTIEF", columnList = "actief"),
		@Index(name = "IDX_MEDEWERKER_ACHTERNAAM", columnList = "achternaam") })
@Audited
@Getter
@Setter
public class Medewerker extends AbstractHibernateObject implements Account, IActief
{
	@Column(unique = true)
	private String agbcode;

	@Column(unique = true, length = 11)
	private String bignummer;

	@Column(unique = true)
	private String uzinummer;

	@Column(length = 20)
	private String voorletters;

	@Column(length = 20)
	private String tussenvoegsel;

	@Column(length = 50)
	private String voornaam;

	@Column(nullable = false, length = 100)
	private String achternaam;

	@OneToMany(mappedBy = "medewerker")
	private List<OrganisatieMedewerker> organisatieMedewerkers = new ArrayList<>();

	@Column(length = 80)
	private String woonplaats;

	@Enumerated(EnumType.STRING)
	private Aanhef aanhef;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private Functie functie;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private Titel titel;

	private String telefoonnummerwerk;

	private String emailwerk;

	private String telefoonnummerextra;

	private String emailextra;

	private String telefoonnummerprive;

	@Temporal(TemporalType.DATE)
	private Date geboortedatum;

	@Column(unique = true, nullable = false)
	private String gebruikersnaam;

	private String wachtwoord;

	@Temporal(TemporalType.TIMESTAMP)
	private Date actiefVanaf;

	@Temporal(TemporalType.TIMESTAMP)
	private Date actiefTotEnMet;

	@Column(unique = true, nullable = false)
	private Integer medewerkercode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date tijdLaatsteFoutieveInlog;

	private Integer foutieveInlogpogingen;

	private InlogStatus inlogstatus;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatsteKeerWachtwoordGewijzigd;

	@Column(nullable = false)
	private boolean wachtwoordVerlooptWaarschuwingVerzonden;

	private String wachtwoordChangeCode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumWachtwoordAanvraag;

	private Boolean actief;

	private Boolean zorgverlener;

	@Column(length = HibernateMagicNumber.L25)
	private String patholoogId;

	@ManyToOne(cascade = jakarta.persistence.CascadeType.ALL)
	@NotAudited
	private YubiKey yubiKey;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private InlogMethode inlogMethode;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "medewerker")
	@NotAudited
	private List<AfgeslotenMedewerkerOvereenkomst> afgeslotenKwaliteitsOvereenkomsten;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "medewerker", cascade = jakarta.persistence.CascadeType.REMOVE)
	private List<MedewerkerNieuwsItem> medewerkerNieuwsItems;

	@OneToOne(fetch = FetchType.LAZY)
	private UploadDocument handtekening;

	@Column
	private String ondertekenaar;

	@Override
	public String toString()
	{
		return this.getNaamVolledig();
	}

	@Transient
	public String getNaamVolledig()
	{
		StringBuffer naamVolledig = new StringBuffer();

		naamVolledig.append(getAchternaam());
		naamVolledig.append(", ");

		if (StringUtils.isNotBlank(getVoorletters()))
		{
			naamVolledig.append(getVoorletters());
			naamVolledig.append(" ");
		}
		else if (StringUtils.isNotBlank(getVoornaam()))
		{
			naamVolledig.append(getVoornaam());
			naamVolledig.append(" ");
		}

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			naamVolledig.append(getTussenvoegsel());
			naamVolledig.append(" ");
		}

		return naamVolledig.toString();
	}

	@Transient
	public String getNaamVolledigMetVoornaam()
	{
		StringBuilder naamVolledig = new StringBuilder();

		naamVolledig.append(getAchternaam());

		if (StringUtils.isNotBlank(getVoorletters()))
		{
			naamVolledig.append(", ");
			naamVolledig.append(getVoorletters());
		}

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			naamVolledig.append(StringUtils.isNotBlank(getVoorletters()) ? " " : ", ");
			naamVolledig.append(getTussenvoegsel());
		}

		if (StringUtils.isNotBlank(getVoornaam()))
		{
			naamVolledig.append(" (");
			naamVolledig.append(getVoornaam());
			naamVolledig.append(")");
		}

		return naamVolledig.toString();
	}

	@Transient
	public String getVoornaamAchternaam()
	{
		StringBuffer voornaamAchternaam = new StringBuffer();
		if (StringUtils.isNotBlank(getVoornaam()))
		{
			voornaamAchternaam.append(getVoornaam());
			voornaamAchternaam.append(" ");
		}
		else if (StringUtils.isNotBlank(getVoorletters()))
		{
			voornaamAchternaam.append(getVoorletters());
			voornaamAchternaam.append(" ");
		}

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			voornaamAchternaam.append(getTussenvoegsel());
			voornaamAchternaam.append(" ");
		}

		voornaamAchternaam.append(getAchternaam());

		return voornaamAchternaam.toString();
	}

	@Transient
	public String getAchternaamVolledig()
	{
		StringBuffer naamVolledig = new StringBuffer();

		naamVolledig.append(getAchternaam());
		naamVolledig.append(", ");

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			naamVolledig.append(getTussenvoegsel());
			naamVolledig.append(" ");
		}

		return naamVolledig.toString();
	}

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
			result = prime * result + (getNaamVolledig() == null ? 0 : getNaamVolledig().hashCode());
			result = prime * result + (getUzinummer() == null ? 0 : getUzinummer().hashCode());
			result = prime * result + (getGebruikersnaam() == null ? 0 : getGebruikersnaam().hashCode());
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
			Medewerker other = (Medewerker) obj;
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
				if (getNaamVolledig() == null)
				{
					if (other.getNaamVolledig() != null)
					{
						returnValue = false;
					}
				}
				else if (!getNaamVolledig().equals(other.getNaamVolledig()))
				{
					returnValue = false;
				}
				if (getGebruikersnaam() == null)
				{
					if (other.getGebruikersnaam() != null)
					{
						returnValue = false;
					}
				}
				else if (!getGebruikersnaam().equals(other.getGebruikersnaam()))
				{
					returnValue = false;
				}
				if (getUzinummer() == null)
				{
					if (other.getUzinummer() != null)
					{
						returnValue = false;
					}
				}
				else if (!getUzinummer().equals(other.getUzinummer()))
				{
					returnValue = false;
				}
			}
		}

		return returnValue;
	}

}
