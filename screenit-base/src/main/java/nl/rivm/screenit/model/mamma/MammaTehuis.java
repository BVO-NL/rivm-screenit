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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.util.DiffSpecs;
import nl.rivm.screenit.util.SkipFieldForDiff;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.envers.Audited;

@Entity
@Table(schema = "mamma", name = "tehuis")
@Audited
@Getter
@Setter
public class MammaTehuis extends AbstractHibernateObject implements IActief
{
	@Column(nullable = false, length = HibernateMagicNumber.L255)
	private String naam;

	@Column(nullable = false, length = HibernateMagicNumber.L255)
	private String contactpersoon;

	@Column(nullable = false)
	private Boolean actief;

	@ManyToOne(optional = false, fetch = FetchType.LAZY)
	@DiffSpecs(displayProperty = "naam")
	private MammaStandplaats standplaats;

	@OneToMany(mappedBy = "tehuis", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaDossier> dossiers = new ArrayList<>();

	@OneToOne(optional = false, fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private Adres aanschrijfAdres;

	@OneToMany(mappedBy = "tehuis", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaTehuisAdres> adressen = new ArrayList<>();

	@OneToMany(mappedBy = "tehuis", fetch = FetchType.LAZY)
	@SkipFieldForDiff
	private List<MammaTehuisOpmerking> opmerkingen = new ArrayList<>();

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String telefoonnummer;

	@Column(nullable = true)
	@Temporal(TemporalType.DATE)
	private Date uitgenodigd;

	@Override
	public Boolean getActief()
	{
		return actief;
	}
}
