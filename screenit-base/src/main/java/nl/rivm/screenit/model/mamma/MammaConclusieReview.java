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

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingMbber;
import nl.rivm.screenit.model.mamma.enums.MammaLezingRedenenFotobesprekingRadioloog;
import nl.rivm.screenit.model.mamma.enums.MammaRetourCeReden;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Getter
@Setter
@Audited
@Table(
	schema = "mamma",
	name = "conclusie_review",
	uniqueConstraints = @UniqueConstraint(columnNames = { "screening_ronde", "radioloog", "reviewAlsCoordinerendRadioloog" }))
public class MammaConclusieReview extends AbstractHibernateObject
{
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaScreeningRonde screeningRonde;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private OrganisatieMedewerker radioloog;

	@Column(nullable = false)
	private Boolean reviewAlsCoordinerendRadioloog = false;

	@Column
	private LocalDateTime reviewMoment;

	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	private MammaRetourCeReden retourCeReden;

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingRadioloog.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@CollectionTable(schema = "mamma", name = "conclusie_review_redenen_fotobespreking_radioloog")
	private List<MammaLezingRedenenFotobesprekingRadioloog> redenenFotobesprekingRadioloog = new ArrayList<>();

	@ElementCollection(targetClass = MammaLezingRedenenFotobesprekingMbber.class)
	@Enumerated(EnumType.STRING)
	@Column(nullable = true)
	@CollectionTable(schema = "mamma", name = "conclusie_review_redenen_fotobespreking_mbber")
	private List<MammaLezingRedenenFotobesprekingMbber> redenenFotobesprekingMbber = new ArrayList<>();

}
