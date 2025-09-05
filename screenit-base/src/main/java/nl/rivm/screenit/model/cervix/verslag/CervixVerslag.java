package nl.rivm.screenit.model.cervix.verslag;

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
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.verslag.VerslagContent;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Table(schema = "cervix", name = "cda_verslag",
	indexes = { @Index(name = "idx_CERVIX_CYTOLOGIE_VERSLAG_CYTOLOGIE_UITSLAG", columnList = "cytologieUitslag") }
)
@Audited
public class CervixVerslag<T extends VerslagContent<?>> extends AbstractHibernateObject implements Verslag<T, CervixScreeningRonde>
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixScreeningRonde screeningRonde;

	@OneToOne(fetch = FetchType.LAZY, optional = false)
	@NotAudited
	private OntvangenCdaBericht ontvangenCdaBericht;

	@ManyToOne(fetch = FetchType.LAZY)
	private Organisatie uitvoerderOrganisatie;

	@ManyToOne(fetch = FetchType.LAZY)
	private Medewerker uitvoerderMedewerker;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVerwerkt;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumOnderzoek;

	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker invoerder;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private VerslagStatus status;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private VerslagType type;

	public T getVerslagContent()
	{
		return null;
	}

	public void setVerslagContent(T verslagContent)
	{

	}

	public OntvangenCdaBericht getOntvangenBericht()
	{
		return ontvangenCdaBericht;
	}

	public void setOntvangenBericht(OntvangenCdaBericht ontvangencdaBericht)
	{
		this.ontvangenCdaBericht = ontvangencdaBericht;
	}
}
