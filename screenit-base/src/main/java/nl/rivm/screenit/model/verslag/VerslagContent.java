
package nl.rivm.screenit.model.verslag;

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

import java.io.Serial;

import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

@Entity
@Table(schema = "gedeeld")
@Inheritance(strategy = InheritanceType.JOINED)
public abstract class VerslagContent<T extends Verslag> extends AbstractHibernateObject
{

	@Serial
	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private VerslagGeneratie versie;

	public VerslagGeneratie getVersie()
	{
		return versie;
	}

	public void setVersie(VerslagGeneratie versie)
	{
		this.versie = versie;
	}

	public abstract T getVerslag();

	public abstract void setVerslag(T colonVerslag);
}
