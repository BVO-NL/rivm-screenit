package nl.rivm.screenit.model.cervix.facturatie;

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
import java.util.List;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Getter
@Setter
@Table(schema = "cervix", name = "betaalopdracht_regel_specificatie")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
public class CervixBetaalopdrachtRegelSpecificatie extends AbstractHibernateObject
{
	@Column(nullable = false)
	private BigDecimal bedrag;

	@Transient
	private transient BigDecimal debets = BigDecimal.ZERO;

	@Column(nullable = false)
	private BigDecimal tariefBedrag;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixBetaalopdrachtRegel betaalopdrachtRegel;

	@OneToMany(mappedBy = "specificatie", fetch = FetchType.LAZY)
	private List<CervixBoekRegel> boekRegels = new ArrayList<>();

	@Enumerated(value = EnumType.STRING)
	private CervixTariefType tariefType;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private CervixTarief tarief;

	@Column(nullable = false)
	private Integer aantalBetaalRegels;
}
