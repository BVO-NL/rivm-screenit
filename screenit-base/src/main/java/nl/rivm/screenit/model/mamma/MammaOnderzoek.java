package nl.rivm.screenit.model.mamma;

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

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
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

import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.enums.ExtraFotosReden;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.SuboptimaleInsteltechniek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.FetchProfile;
import org.hibernate.envers.Audited;

@Entity
@Getter
@Setter
@Table(
	schema = "mamma",
	name = "onderzoek",
	uniqueConstraints = { @UniqueConstraint(columnNames = "mammografie"), @UniqueConstraint(columnNames = "signaleren"), @UniqueConstraint(columnNames = "laatste_beoordeling") },
	indexes = {
		@Index(name = "idx_mamma_onderzoek_status", columnList = "status"),
	})
@Audited
@FetchProfile(
	name = "kansberekening",
	fetchOverrides = {
		@FetchProfile.FetchOverride(entity = MammaOnderzoek.class, association = "laatsteBeoordeling", mode = FetchMode.JOIN),
		@FetchProfile.FetchOverride(entity = MammaOnderzoek.class, association = "mammografie", mode = FetchMode.JOIN),
	})
public class MammaOnderzoek extends AbstractHibernateObject
{
	@OneToOne(optional = false, mappedBy = "onderzoek")
	private MammaAfspraak afspraak;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaScreeningsEenheid screeningsEenheid;

	@OneToMany(mappedBy = "onderzoek", fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE,
		jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private List<MammaBeoordeling> beoordelingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, cascade = jakarta.persistence.CascadeType.ALL)
	private MammaBeoordeling laatsteBeoordeling;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE,
		jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaMammografie mammografie;

	@OneToOne(fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.PERSIST, jakarta.persistence.CascadeType.MERGE,
		jakarta.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaSignaleren signaleren;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekStatus status;

	@ManyToOne(fetch = FetchType.LAZY)
	private ZorgInstelling eerderMammogramZorginstelling;

	@Column
	private Integer eerderMammogramJaartal;

	@Column()
	@Enumerated(EnumType.STRING)
	private SuboptimaleInsteltechniek suboptimaleInsteltechniek;

	@Column()
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekRedenFotobespreking redenFotobespreking;

	@Column()
	private String opmerkingMbber;

	@Column()
	private String opmerkingVoorRadioloog;

	@Column(nullable = false)
	private Boolean operatieRechts;

	@Column(nullable = false)
	private Boolean operatieLinks;

	@Column
	@Enumerated(EnumType.STRING)
	private MammaAmputatie amputatie;

	@Column
	private String aanvullendeInformatieOperatie;

	@Column(nullable = false)
	private boolean huidscheuring;

	@Column
	@Enumerated(EnumType.STRING)
	private OnvolledigOnderzoekOption onvolledigOnderzoek;

	@Column
	@Enumerated(EnumType.STRING)
	private OnderbrokenOnderzoekOption onderbrokenOnderzoek;

	@ElementCollection(targetClass = ExtraFotosReden.class)
	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "mamma", name = "extra_fotos_reden")
	private List<ExtraFotosReden> extraFotosRedenen = new ArrayList<>();

	@Column
	private String adviesHuisarts;

	@Column
	@Temporal(TemporalType.TIMESTAMP)
	private Date afgerondOp;

	@ManyToOne(fetch = FetchType.LAZY)
	private OrganisatieMedewerker extraMedewerker;

	@Column(nullable = false)
	private boolean isDoorgevoerd;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekType onderzoekType;

	@OneToOne(mappedBy = "onderzoek")
	private MammaAdhocMeekijkverzoek meekijkverzoek;
}
