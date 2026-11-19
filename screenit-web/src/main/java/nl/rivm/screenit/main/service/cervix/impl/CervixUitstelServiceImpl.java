package nl.rivm.screenit.main.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDate;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.dto.algemeen.CervixUitstelAanvragenDto;
import nl.rivm.screenit.main.dto.algemeen.CervixUitstelDto;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.main.mappers.cervix.CervixUitstelMapper;
import nl.rivm.screenit.main.service.cervix.CervixUitstelService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.preference.service.SimplePreferenceService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class CervixUitstelServiceImpl implements CervixUitstelService
{
	private static final int MAXIMAAL_AANTAL_MAANDEN_ZWANGERSCHAP = 9;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final CervixUitstelMapper cervixUitstelMapper;

	@Override
	public CervixUitstelDto getHuidigUitstel(Client client)
	{
		var ronde = client.getCervixDossier().getLaatsteScreeningRonde();
		return cervixUitstelMapper.naarDto(ronde);
	}

	@Override
	public CervixUitstel maakUitstel(Client client, CervixUitstelAanvragenDto dto)
	{
		var ronde = client.getCervixDossier().getLaatsteScreeningRonde();
		var uitstel = ronde.getUitstel();
		if (uitstel == null)
		{
			uitstel = new CervixUitstel();
			uitstel.setScreeningRonde(ronde);
		}
		uitstel.setUitstelType(dto.getUitstelType());
		uitstel.setUitstellenTotDatum(DateUtil.toUtilDate(bepaalUitstellenTotDatum(dto.getUitstelType(), dto.getDatum())));
		return uitstel;
	}

	@Override
	public void valideerUitstel(Client client, CervixUitstelType uitstelType, LocalDate datum) throws ValidatieException
	{
		if (uitstelType == null)
		{
			throw new ValidatieException("error.uitstel.cervix.geen.reden");
		}
		if (datum == null)
		{
			throw new ValidatieException("error.uitstel.cervix.geen.datum");
		}

		var dagenUitstelBijZwangerschap = getDagenUitstelBijZwangerschap();
		var morgen = currentDateSupplier.getLocalDate().plusDays(1);
		var uitstellenTotDatum = bepaalUitstellenTotDatum(uitstelType, datum);
		var dagNaDertigsteVerjaardag = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum()).plusYears(30).plusDays(1);

		if (uitstelType == CervixUitstelType.ZWANGERSCHAP)
		{
			if (uitstellenTotDatum.isBefore(morgen))
			{
				throw new ValidatieException("error.uitstel.cervix.bevallingsdatum.te.oud", dagenUitstelBijZwangerschap);
			}
			if (morgen.plusMonths(MAXIMAAL_AANTAL_MAANDEN_ZWANGERSCHAP).minusDays(1).isBefore(datum))
			{
				throw new ValidatieException("error.uitstel.cervix.bevallingsdatum.meer.dan.negen.maanden");
			}
			if (uitstellenTotDatum.isBefore(dagNaDertigsteVerjaardag))
			{
				throw new ValidatieException("error.uitstel.cervix.bevallingsdatum.voor.dertigste.verjaardag",
					dagNaDertigsteVerjaardag.minusDays(dagenUitstelBijZwangerschap).format(DateUtil.LOCAL_DATE_FORMAT));
			}
		}
		else
		{
			if (uitstellenTotDatum.isBefore(morgen))
			{
				throw new ValidatieException("error.uitstel.cervix.datum.niet.in.toekomst");
			}
			if (uitstellenTotDatum.isBefore(dagNaDertigsteVerjaardag))
			{
				throw new ValidatieException("error.uitstel.cervix.datum.voor.dertigste.verjaardag");
			}
		}

		var volgendeRondeVanaf = client.getCervixDossier().getVolgendeRondeVanaf();
		if (volgendeRondeVanaf != null && volgendeRondeVanaf.before(DateUtil.toUtilDate(uitstellenTotDatum)))
		{
			throw new ValidatieException("error.uitstel.cervix.loopt.af.na.volgende.ronde");
		}
	}

	private int getDagenUitstelBijZwangerschap()
	{
		return preferenceService.getInteger(PreferenceKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX.name());
	}

	private LocalDate bepaalUitstellenTotDatum(CervixUitstelType uitstelType, LocalDate invoerdatum)
	{
		return uitstelType == CervixUitstelType.ZWANGERSCHAP ? invoerdatum.plusDays(getDagenUitstelBijZwangerschap()) : invoerdatum;
	}
}
