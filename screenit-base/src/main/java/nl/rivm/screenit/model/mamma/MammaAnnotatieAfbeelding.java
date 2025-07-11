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

import java.io.Serial;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "mamma", name = "annotatie_afbeelding")
@Audited
public class MammaAnnotatieAfbeelding extends AbstractHibernateObject
{
	@Serial
	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "afbeelding", fetch = FetchType.LAZY, cascade = { jakarta.persistence.CascadeType.REMOVE, jakarta.persistence.CascadeType.PERSIST,
		jakarta.persistence.CascadeType.MERGE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	@NotAudited
	private List<MammaAnnotatieIcoon> iconen = new ArrayList<>();

	public List<MammaAnnotatieIcoon> getIconen()
	{
		return iconen;
	}

	public void setIconen(List<MammaAnnotatieIcoon> iconen)
	{
		this.iconen = iconen;
	}
}
