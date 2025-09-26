
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
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table
@Audited
@Getter
@Setter
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class Brief extends AbstractHibernateObject
{
	@Serial
	private static final long serialVersionUID = 1L;

	@ManyToOne(optional = true, fetch = FetchType.LAZY)
	@Audited(targetAuditMode = NOT_AUDITED)
	private BriefDefinitie briefDefinitie;

	@Enumerated(EnumType.STRING)
	private BriefType briefType;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date creatieDatum;

	private boolean gegenereerd;

	@Column(nullable = false)
	private boolean vervangen = false;

	@Column(nullable = false)
	private boolean tegenhouden = false;

	private String templateNaam;

	public abstract MergedBrieven getMergedBrieven();

	public abstract void setMergedBrieven(MergedBrieven mergedBrieven);

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		Class aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (aClass == ColonBrief.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (aClass == CervixBrief.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (aClass == MammaBrief.class)
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}

}
