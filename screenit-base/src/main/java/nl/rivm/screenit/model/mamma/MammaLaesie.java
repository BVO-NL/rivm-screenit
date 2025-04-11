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

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Check;
import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "laesie")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@Check(constraints = "laesie.laesie_grootte_in_cm >= 0")
@Getter
@Setter
public abstract class MammaLaesie extends SingleTableHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaLezing lezing;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaZijde mammaZijde;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "positieX", column = @Column(name = "verticaleDoorsnedeIcoonPositieX", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)),
		@AttributeOverride(name = "positieY", column = @Column(name = "verticaleDoorsnedeIcoonPositieY", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3))
	})
	private MammaLaesieIcoon verticaleDoorsnedeIcoon;

	@Embedded
	@AttributeOverrides({
		@AttributeOverride(name = "positieX", column = @Column(name = "horizontaleDoorsnedeIcoonPositieX", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3)),
		@AttributeOverride(name = "positieY", column = @Column(name = "horizontaleDoorsnedeIcoonPositieY", precision = HibernateMagicNumber.P6, scale = HibernateMagicNumber.S3))
	})
	private MammaLaesieIcoon horizontaleDoorsnedeIcoon;

	@Column(nullable = false)
	private Integer nummer;

	@Column(nullable = true, length = 3)
	private String laesieVolgorde;

	@Column(precision = 5, scale = 1)
	private BigDecimal laesieGrootteInCm;

	public abstract MammaLaesieType getMammaLaesieType();
}
