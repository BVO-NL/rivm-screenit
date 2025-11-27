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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Getter
@Setter
@Entity
@Audited
public class ScreeningOrganisatie extends Organisatie
{
	@OneToMany(mappedBy = "screeningOrganisatie", cascade = CascadeType.ALL)
	private List<Gemeente> gemeentes = new ArrayList<>();

	@OneToMany(mappedBy = "regio", fetch = FetchType.LAZY)
	private List<ZASRetouradres> retouradressen = new ArrayList<>();

	private Integer fitRetourPercentage;

	private String rcmdl;

	@Column(length = 512)
	private String vertegenwoordiger;

	private String rechtbank;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_logo_inpakcentrum")
	private UploadDocument logo;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_logo_brief")
	private UploadDocument logoBrief;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_bestuur_sign")
	private UploadDocument bestuurSign;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "so_rcmdl_sign")
	private UploadDocument rcmdlSign;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument kwaliteitslogo;

	@Column(unique = true, length = 2)
	private String regioCode;

	@Column(length = HibernateMagicNumber.L256)
	private String clientPortaalVrijeTekst;

	private String enovationKlantnummer;

	private String enovationEdiAdres;

	@Column(length = 34)
	private String iban;

	private Integer afspraakDrempelBk = 10;

	@Column(length = 70)
	private String ibanTenaamstelling;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private RegioBvoContactGegevens regioBvoContactGegevensDk;

	@NotAudited
	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
	private RegioBvoContactGegevens regioBvoContactGegevensBmhk;

	@Column(precision = HibernateMagicNumber.P3, scale = HibernateMagicNumber.S2)
	private BigDecimal factorEersteOnderzoekBk;

	@Column(precision = HibernateMagicNumber.P3, scale = HibernateMagicNumber.S2)
	private BigDecimal factorDubbeleTijdBk;

	@Column(precision = HibernateMagicNumber.P3, scale = HibernateMagicNumber.S2)
	private BigDecimal factorMinderValideBk;

	@Column
	private Integer wekenVanTevorenUitnodigen;

	@Column
	private Integer vervallenCapaciteitsreserveringDagenBk;

	@Column
	private Integer minimaleDagCapaciteitMinderValideAfspraken;
}
