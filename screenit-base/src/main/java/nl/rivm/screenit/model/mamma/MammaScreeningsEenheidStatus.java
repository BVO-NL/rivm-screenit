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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

@Entity
@Table(
	schema = "mamma",
	name = "screenings_eenheid_status")
@Getter
@Setter
public class MammaScreeningsEenheidStatus extends AbstractHibernateObject
{

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	private MammaScreeningsEenheid screeningsEenheid;

	@Column(nullable = false)
	private String versie;

	@Column(nullable = false)
	private Boolean huisartsenAanwezig;

	@Column(nullable = false)
	private Boolean zorginstellingenAanwezig;

	@Column(nullable = false)
	private Boolean mammografenAanwezig;

	@Column(nullable = true)
	private String offlineDaglijsten;

	@Column(nullable = true)
	private LocalDateTime laatsteKeerOfflineGegaan;

	@Column(nullable = false)
	private LocalDateTime statusMoment;
}
