package nl.rivm.screenit.model;

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
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.DigitaalBerichtTemplateType;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.envers.Audited;

@Entity
@Table
@Audited
@Getter
@Setter
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class DigitaalClientBericht<SR extends ScreeningRonde> extends AbstractHibernateObject
{
	@Column(nullable = false)
	private LocalDateTime creatieMoment;

	@Column(nullable = false)
	private Boolean isHerzonden = false;

	@Column(nullable = false, length = GbaPersoon.MAX_EMAIL_LENGTH)
	private String ontvanger;

	@Column(nullable = false)
	private Boolean verzendenGefaald;

	@Column(length = 100)
	private String omschrijving;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private DigitaalBerichtTemplateType digitaalBerichtTemplateType;

	public abstract SR getScreeningRonde();

	public abstract void setScreeningRonde(SR screeningRonde);
}
