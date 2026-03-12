package nl.rivm.screenit.model.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.mamma.berichten.MammaHuisartsBericht;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "mamma",
	name = "beoordeling",
	uniqueConstraints = {
		@UniqueConstraint(columnNames = "eerste_lezing"), @UniqueConstraint(columnNames = "tweede_lezing"),
		@UniqueConstraint(columnNames = "discrepantie_lezing"), @UniqueConstraint(columnNames = "arbitrage_lezing"), @UniqueConstraint(columnNames = "verslag_lezing") },
	indexes = { @Index(name = "idx_mamma_beoordeling_status", columnList = "status") })
@Audited
@Getter
@Setter
public class MammaBeoordeling extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing eersteLezing;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing tweedeLezing;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing discrepantieLezing;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing arbitrageLezing;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaLezing verslagLezing;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBeoordelingStatus status = MammaBeoordelingStatus.EERSTE_LEZING;

	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker reserveringhouder;

	@Temporal(TemporalType.TIMESTAMP)
	private Date reserveringsmoment;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaOnderzoek onderzoek;

	@Column
	private String afkeurreden;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private BeoordelingsEenheid beoordelingsEenheid;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBeoordelingOpschortenReden opschortReden = MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN;

	@Column
	private String opschortRedenTekst;

	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker opschortOrganisatieMedewerker;

	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker toegewezenOrganisatieMedewerker;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date toegewezenOp;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "beoordeling")
	private List<MammaHuisartsBericht> huisartsBerichten = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY)
	private UploadDocument verslagPdf;

	@Column
	private String redenAnnuleren;

}
