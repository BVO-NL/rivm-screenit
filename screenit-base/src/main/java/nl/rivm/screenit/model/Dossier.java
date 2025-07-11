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

import java.util.Date;
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.CervixDossier;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table
@Audited
@Getter
@Setter
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class Dossier<SR extends ScreeningRonde<?, ?, ?, ?>, AF extends Afmelding<?, ?, ?>> extends AbstractHibernateObject
{
	@Enumerated(EnumType.STRING)
	private DossierStatus status;

	@Column(nullable = false)
	private Boolean aangemeld;

	@Temporal(TemporalType.DATE)
	private Date inactiefVanaf;

	@Temporal(TemporalType.DATE)
	private Date inactiefTotMet;

	@Temporal(TemporalType.DATE)
	private Date datumLaatstGecontroleerdeSignalering;

	@NotAudited
	@Getter
	@Setter
	@Column(nullable = true)
	private Boolean wachtOpStartProject = false;

	public abstract Client getClient();

	public abstract void setClient(Client client);

	public abstract List<SR> getScreeningRondes();

	public abstract void setScreeningRondes(List<SR> screeningRondes);

	public abstract SR getLaatsteScreeningRonde();

	public abstract void setLaatsteScreeningRonde(SR laatsteScreeningRonde);

	public abstract List<AF> getAfmeldingen();

	public abstract void setAfmeldingen(List<AF> afmeldingen);

	public abstract AF getLaatsteAfmelding();

	public abstract void setLaatsteAfmelding(AF laatsteAfmelding);

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		Class aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (aClass == ColonDossier.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (aClass == CervixDossier.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (aClass == MammaDossier.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}

}
