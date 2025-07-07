package nl.rivm.screenit.model.colon;

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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity(name = "colon.cda_verslag")
@Table(schema = "colon", name = "cda_verslag")
@Proxy
@Audited
public class ColonVerslag<T extends VerslagContent<?>> extends AbstractHibernateObject implements Verslag<T, ColonScreeningRonde>
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private ColonScreeningRonde screeningRonde;

	@OneToOne(fetch = FetchType.LAZY)
	@NotAudited
	private OntvangenCdaBericht ontvangenCdaBericht;

	@ManyToOne(fetch = FetchType.LAZY)
	private Instelling uitvoerderOrganisatie;

	@ManyToOne(fetch = FetchType.LAZY)
	private Gebruiker uitvoerderMedewerker;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVerwerkt;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumOnderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker invoerder;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private VerslagStatus status;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private VerslagType type;

	@Override
	public OntvangenCdaBericht getOntvangenBericht()
	{
		return ontvangenCdaBericht;
	}

	@Override
	public void setOntvangenBericht(OntvangenCdaBericht ontvangencdaBericht)
	{
		this.ontvangenCdaBericht = ontvangencdaBericht;
	}

	@Override
	public T getVerslagContent()
	{
		return null;
	}

	@Override
	public void setVerslagContent(T verslagContent)
	{

	}
}
