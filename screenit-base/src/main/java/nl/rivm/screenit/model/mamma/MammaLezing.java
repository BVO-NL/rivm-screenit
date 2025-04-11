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
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaAfwijkingTeZienOp;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.model.mamma.enums.MammaNevenbevindingen;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.Check;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "lezing")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Getter
@Setter
@Check(
	constraints = "(lezing.lezing_type = 'DISCREPANTIE_LEZING' AND lezing.birads_links = 'GEEN' AND lezing.birads_rechts = 'GEEN') "
		+ "OR ((lezing.birads_links != 'GEEN' OR lezing.birads_rechts != 'GEEN') AND lezing.beoordelaar IS NOT NULL) " +
		"OR (lezing.birads_links = 'GEEN' AND lezing.birads_rechts = 'GEEN' AND lezing.beperkt_beoordeelbaar_reden = 'GEEN_BEOORDELING_MOGELIJK')")
public class MammaLezing extends AbstractHibernateObject
{
	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date beoordelingDatum;

	@Transient 
	private MammaBeoordeling beoordeling;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker beoordelaar;

	private boolean onervarenRadioloog;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBIRADSWaarde biradsLinks;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaBIRADSWaarde biradsRechts;

	@Column(length = HibernateMagicNumber.L255)
	private String biradsOpmerking;

	@OneToMany(mappedBy = "lezing", fetch = FetchType.LAZY, orphanRemoval = true, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private List<MammaLaesie> laesies = new ArrayList<>();

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaLezingType lezingType;

	@Column(length = HibernateMagicNumber.L255)
	@Enumerated(EnumType.STRING)
	private MammaBeperktBeoordeelbaarReden beperktBeoordeelbaarReden;

	@Column(length = HibernateMagicNumber.L255)
	private String waaromGeenBeoordelingMogelijk;

	@ElementCollection(targetClass = MammaNevenbevindingen.class, fetch = FetchType.EAGER)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "lezing_nevenbevindingen")
	private List<MammaNevenbevindingen> nevenbevindingen = new ArrayList<>();

	@Column(length = HibernateMagicNumber.L255, nullable = true)
	private String nevenbevindingOpmerking;

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingRadioloog.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "lezing_redenen_fotobespreking_radioloog")
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobesprekingRadioloog = new ArrayList<>();

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingMbber.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
	@CollectionTable(schema = "mamma", name = "lezing_redenen_fotobespreking_mbber")
	private List<MammaLezingRedenenFotobesprekingMbber> redenenFotobesprekingMbber = new ArrayList<>();

	@Column
	private Boolean tomosyntheseRelevantVoorBeoordeling;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private MammaAfwijkingTeZienOp afwijkingTeZienOp;

	@Column(nullable = true)
	private Integer zichtbaarOpCcNummer;

	@Column(nullable = true)
	private Integer zichtbaarOpMloNummer;

	@Transient
	public MammaBeoordeling getBeoordeling()
	{
		return beoordeling;
	}

	@Transient
	public void setBeoordeling(MammaBeoordeling beoordeling)
	{
		this.beoordeling = beoordeling;
	}
}
