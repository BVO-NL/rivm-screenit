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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Index;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	schema = "mamma",
	name = "capaciteit_blok",
	indexes = { @Index(name = "idx_mamma_capaciteit_blok_vanaf", columnList = "vanaf"),
		@Index(name = "idx_mamma_capaciteit_blok_tot", columnList = "tot"), @Index(name = "idx_mamma_capaciteit_blok_blok_type", columnList = "blokType") })
@Audited
@Getter
@Setter
@NoArgsConstructor
public class MammaCapaciteitBlok extends AbstractHibernateObject
{
	public MammaCapaciteitBlok(MammaCapaciteitBlok copy, int daysOffSet)
	{
		this.aantalOnderzoeken = copy.aantalOnderzoeken;
		this.blokType = copy.blokType;
		this.opmerkingen = copy.opmerkingen;
		this.screeningsEenheid = copy.screeningsEenheid;
		this.vanaf = DateUtil.plusDagen(copy.vanaf, daysOffSet);
		this.tot = DateUtil.plusDagen(copy.tot, daysOffSet);
		this.minderValideAfspraakMogelijk = copy.minderValideAfspraakMogelijk;
	}

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningsEenheid screeningsEenheid;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date vanaf;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date tot;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private MammaCapaciteitBlokType blokType;

	@Column(nullable = false)
	private Integer aantalOnderzoeken;

	@Column
	private String opmerkingen;

	@Transient
	private BigDecimal beschikbareCapaciteit;

	@Transient
	private BigDecimal vrijeCapaciteit;

	@Column(nullable = false)
	private Boolean minderValideAfspraakMogelijk;

	@OneToMany(mappedBy = "capaciteitBlok", fetch = FetchType.LAZY)
	private final List<MammaAfspraak> afspraken = new ArrayList<>();

	@OneToMany(mappedBy = "capaciteitBlok", fetch = FetchType.LAZY, cascade = CascadeType.REMOVE)
	@Cascade(org.hibernate.annotations.CascadeType.DELETE)
	@NotAudited
	private List<MammaAfspraakReservering> afspraakReserveringen;

}
