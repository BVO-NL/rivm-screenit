
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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.BriefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Audited
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class MergedBrieven<B extends Brief> extends AbstractHibernateObject implements IActief
{
	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date creatieDatum;

	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@ManyToOne
	@NotAudited
	private UploadDocument mergedBrieven;

	@Column(nullable = false)
	private Boolean geprint = false;

	@Column(nullable = false)
	private Boolean controle = false;

	@Column(nullable = false)
	private Boolean vrijgegeven = false;

	private boolean verwijderd;

	@Temporal(TemporalType.TIMESTAMP)
	private Date printDatum;

	@ManyToOne
	private Medewerker afgedruktDoor;

	@ManyToOne
	private ScreeningOrganisatie screeningOrganisatie;

	@Column(nullable = false)
	private Integer aantalBrieven = Integer.valueOf(0);

	@Temporal(TemporalType.TIMESTAMP)
	private Date controleerDatum;

	@Override
	@Transient
	public Boolean getActief()
	{
		return getGeprint();
	}

	@Override
	@Transient
	public void setActief(Boolean actief)
	{
		setGeprint(actief);
	}

	public abstract List<B> getBrieven();

	public abstract void setBrieven(List<B> brieven);
}
