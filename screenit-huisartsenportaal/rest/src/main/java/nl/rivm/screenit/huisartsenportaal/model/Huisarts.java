package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal-rest
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

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;

import org.hibernate.annotations.LazyCollection;
import org.hibernate.annotations.LazyCollectionOption;
import org.hibernate.envers.Audited;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;

@Entity
@Audited
@Getter
@Setter
public class Huisarts extends Medewerker
{
	private String agbcode;

	private String email;

	private String aanhef;

	@Column(length = 100)
	private String achternaam;

	@Column(length = 20)
	private String tussenvoegsel;

	@Column(length = 20)
	private String voorletters;

	@Column(length = 25)
	private String telefoon;

	private String extraEmails;

	@Enumerated(EnumType.STRING)
	private AanmeldStatus aanmeldStatus;

	@OneToMany(mappedBy = "huisarts")
	@LazyCollection(LazyCollectionOption.FALSE)
	private List<Locatie> locaties = new ArrayList<Locatie>();

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "huisarts")
	private List<LabformulierAanvraag> aanvragen = new ArrayList<LabformulierAanvraag>();

	@OneToOne(fetch = FetchType.EAGER)
	private Adres postadres;

	@Temporal(TemporalType.TIMESTAMP)
	private Date overeenkomstGeaccordeerdDatum;

}
