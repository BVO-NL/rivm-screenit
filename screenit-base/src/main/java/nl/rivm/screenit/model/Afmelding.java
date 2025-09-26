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

import jakarta.persistence.CascadeType;
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

import nl.rivm.screenit.model.cervix.CervixAfmelding;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.mamma.MammaAfmelding;
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
public abstract class Afmelding<SR extends ScreeningRonde<?, ?, ?, ?>, D extends Dossier<?, ?>, B extends ClientBrief<?, ?, ?>> extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date afmeldDatum;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AfmeldingType type;

	@Transient
	private ClientContactManier manier;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private AanvraagBriefStatus afmeldingStatus;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusAfmeldDatum;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	@NotAudited
	private UploadDocument handtekeningDocumentAfmelding;

	@Column(nullable = false)
	private Boolean rondeGesloten = false;

	@Column(nullable = false)
	private Boolean rondeHeropend = false;

	@Temporal(TemporalType.TIMESTAMP)
	private Date heraanmeldDatum;

	@Enumerated(EnumType.STRING)
	private AanvraagBriefStatus heraanmeldStatus;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusHeraanmeldDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = true, cascade = CascadeType.ALL)
	@NotAudited
	private UploadDocument handtekeningDocumentHeraanmelding;

	private Boolean clientWilNieuweUitnodiging;

	@Column(nullable = false)
	private Boolean implicieteAfmelding = false;

	@Column(nullable = false)
	private Boolean implicieteHeraanmelding = false;

	public abstract B getAfmeldingAanvraag();

	public abstract void setAfmeldingAanvraag(B afmeldingAanvraag);

	public abstract B getAfmeldingBevestiging();

	public abstract void setAfmeldingBevestiging(B afmeldingBevestiging);

	public abstract B getHeraanmeldAanvraag();

	public abstract void setHeraanmeldAanvraag(B heraanmeldAanvraag);

	public abstract B getHeraanmeldBevestiging();

	public abstract void setHeraanmeldBevestiging(B heraanmeldBevestiging);

	public abstract List<B> getBrieven();

	public abstract void setBrieven(List<B> brieven);

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);

	public abstract D getDossier();

	public abstract void setDossier(D dossier);

	@Transient
	public Bevolkingsonderzoek getBevolkingsonderzoek()
	{
		var aClass = Hibernate.getClass(this);
		Bevolkingsonderzoek bevolkingsonderzoek = null;
		if (ColonAfmelding.class.isAssignableFrom(aClass))
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.COLON;
		}
		else if (CervixAfmelding.class.isAssignableFrom(aClass))
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.CERVIX;
		}
		else if (MammaAfmelding.class.isAssignableFrom(aClass))
		{
			bevolkingsonderzoek = Bevolkingsonderzoek.MAMMA;
		}
		return bevolkingsonderzoek;
	}
}
