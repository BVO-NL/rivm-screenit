package nl.rivm.screenit.mamma.planning.repository.projectie;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import lombok.Getter;

import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaUitnodigingsintervalType;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.util.DateUtil;

@Getter
public class ClientProjectie
{
	private Long id;

	private Date geboortedatum;

	private String postcode;

	private String tijdelijkGbaPostcode;

	private Long screeningOrganisatieId;

	private Long dossierId;

	private MammaDoelgroep doelgroep;

	private Long tehuisId;

	private Boolean eersteOnderzoek;

	private Date laatsteMammografieAfgerondOp;

	private BigDecimal deelnamekans;

	private Date screeningRondeCreatieDatum;

	private Long oorspronkelijkeStandplaatsRondeId;

	private Boolean screeningRondeIsGeforceerd;

	private Long uitstelStandplaatsId;

	private Date uitstelStreefDatum;

	private MammaUitstelReden uitstelReden;

	private Long uitstelUitnodigingId;

	private Long uitnodigingStandplaatsRondeId;

	private Long afspraakStandplaatsRondeId;

	private Date afspraakAfgezegdOp;

	private Integer voorgaandeScreeningRondes;

	private Date laatsteUitnodigingDatum;

	private MammaAfspraakStatus afspraakStatus;

	private Date afspraakMoment;

	private MammaUitnodigingsintervalType uitnodigingsIntervalType;

	public LocalDate getGeboortedatum()
	{
		return DateUtil.toLocalDate(geboortedatum);
	}

	public LocalDateTime getLaatsteMammografieAfgerondOp()
	{
		return DateUtil.toLocalDateTime(laatsteMammografieAfgerondOp);
	}

	public LocalDateTime getScreeningRondeCreatieDatum()
	{
		return DateUtil.toLocalDateTime(screeningRondeCreatieDatum);
	}

	public LocalDate getUitstelStreefDatum()
	{
		return DateUtil.toLocalDate(uitstelStreefDatum);
	}

	public LocalDateTime getAfspraakAfgezegdOp()
	{
		return DateUtil.toLocalDateTime(afspraakAfgezegdOp);
	}

	public LocalDateTime getLaatsteUitnodigingDatum()
	{
		return DateUtil.toLocalDateTime(laatsteUitnodigingDatum);
	}

	public LocalDateTime getAfspraakMoment()
	{
		return DateUtil.toLocalDateTime(afspraakMoment);
	}
}
