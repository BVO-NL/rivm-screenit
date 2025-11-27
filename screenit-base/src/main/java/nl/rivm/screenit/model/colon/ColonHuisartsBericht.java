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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.HuisartsBericht;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity
@Table(schema = "colon", name = "huisarts_bericht")
@Audited
public class ColonHuisartsBericht extends HuisartsBericht
{
	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST, optional = false)
	@NotAudited
	private EnovationHuisarts huisarts;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST, optional = false)
	private ColonScreeningRonde screeningsRonde;

	@Enumerated(EnumType.STRING)
	private ColonHuisartsBerichtStatus status;

	private Date verzendDatum;
}
