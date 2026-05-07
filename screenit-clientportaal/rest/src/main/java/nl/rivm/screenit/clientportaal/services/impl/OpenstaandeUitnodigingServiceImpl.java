package nl.rivm.screenit.clientportaal.services.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-rest
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

import java.util.EnumMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.clientportaal.services.OpenstaandeUitnodigingService;
import nl.rivm.screenit.clientportaal.services.cervix.CervixUitnodigingService;
import nl.rivm.screenit.clientportaal.services.cervix.CervixZasService;
import nl.rivm.screenit.clientportaal.services.colon.ColonFitService;
import nl.rivm.screenit.clientportaal.services.colon.ColonUitnodigingService;
import nl.rivm.screenit.factory.algemeen.BriefFactory;
import nl.rivm.screenit.mappers.OpenstaandeUitnodigingMapper;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.uitnodiging.dto.CervixUitnodigingKeuzeResponse;
import nl.rivm.screenit.model.uitnodiging.dto.ColonUitnodigingKeuzeResponse;
import nl.rivm.screenit.model.uitnodiging.dto.OpenstaandeUitnodigingDto;
import nl.rivm.screenit.model.uitnodiging.dto.OpenstaandeUitnodigingMeedoenRequest;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.BriefHerdrukkenService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class OpenstaandeUitnodigingServiceImpl implements OpenstaandeUitnodigingService
{
	private final ColonFitService colonFitService;

	private final CervixZasService cervixZasService;

	private final BriefFactory briefFactory;

	private final BriefHerdrukkenService briefHerdrukkenService;

	private final ColonUitnodigingService colonUitnodigingService;

	private final CervixUitnodigingService cervixUitnodigingService;

	private final OpenstaandeUitnodigingMapper mapper;

	private final BaseHoudbaarheidService houdbaarheidService;

	@Override
	@Transactional
	public boolean verwerkMeedoen(Client client, OpenstaandeUitnodigingMeedoenRequest request)
	{
		var openstaandeUitnodigingen = getOpenstaandeUitnodigingen(client);
		if (openstaandeUitnodigingen == null || openstaandeUitnodigingen.isEmpty())
		{
			LOG.warn("Geen openstaande uitnodigingen gevonden voor client {}, verzoek om mee te doen kan niet worden verwerkt", client.getId());
			return false;
		}

		var uitnodigingPerBevolkingsonderzoek = openstaandeUitnodigingen.stream()
			.collect(Collectors.toMap(OpenstaandeUitnodigingDto::bevolkingsonderzoekType, dto -> dto, (eerste, _) -> eerste,
				() -> new EnumMap<>(Bevolkingsonderzoek.class)));

		verwerkCervix(client, request, uitnodigingPerBevolkingsonderzoek.get(Bevolkingsonderzoek.CERVIX));
		verwerkColon(client, request, uitnodigingPerBevolkingsonderzoek.get(Bevolkingsonderzoek.COLON));

		return true;
	}

	@Override
	public List<OpenstaandeUitnodigingDto> getOpenstaandeUitnodigingen(Client client)
	{
		var colonUitnodiging = colonUitnodigingService.getOpenstaandeUitnodiging(client);
		var cervixUitnodiging = cervixUitnodigingService.getOpenstaandeUitnodiging(client);

		var colonUitnodigingDto = Optional.ofNullable(colonUitnodiging)
			.map(uitnodiging -> mapper.mapFromColonUitnodiging(uitnodiging, houdbaarheidService))
			.orElse(null);
		var cervixUitnodigingDto = Optional.ofNullable(cervixUitnodiging)
			.map(uitnodiging -> mapper.mapFromCervixUitnodiging(uitnodiging, houdbaarheidService))
			.orElse(null);

		return Stream.of(colonUitnodigingDto, cervixUitnodigingDto)
			.filter(java.util.Objects::nonNull)
			.toList();
	}

	private void verwerkCervix(Client client, OpenstaandeUitnodigingMeedoenRequest request, OpenstaandeUitnodigingDto cervixDto)
	{
		if (cervixDto == null || request.cervixKeuze() == null || (request.cervixKeuze() == CervixUitnodigingKeuzeResponse.HEB_NOG))
		{
			return;
		}
		switch (request.cervixKeuze())
		{
		case AANVRAGEN_ZELFTEST -> vraagNieuweZasAan(client);
		case AANVRAGEN_BRIEF -> vraagNieuweBriefAan(client);
		default -> LOG.warn("Onverwachte CervixKeuze: {}", request.cervixKeuze());
		}
	}

	private void vraagNieuweBriefAan(Client client)
	{
		var laatsteScreeningRonde = client.getCervixDossier().getLaatsteScreeningRonde();
		if (laatsteScreeningRonde == null)
		{
			LOG.warn("Kan geen nieuwe brief aanmaken voor client {}, geen screeningronde gevonden", client.getId());
			return;
		}
		var nieuweBrief = briefFactory.maakBvoBrief(laatsteScreeningRonde, BriefType.CERVIX_HERINNERING_ZAS_UITNODIGING);
		briefHerdrukkenService.opnieuwAanmaken(nieuweBrief, client);
	}

	private void vraagNieuweZasAan(Client client)
	{
		cervixZasService.vraagZasAan(client, false);
	}

	private void verwerkColon(Client client, OpenstaandeUitnodigingMeedoenRequest request, OpenstaandeUitnodigingDto colonDto)
	{
		if (colonDto == null || request.colonKeuze() == null)
		{
			return;
		}
		if (request.colonKeuze() == ColonUitnodigingKeuzeResponse.AANVRAGEN_NIEUW)
		{
			colonFitService.vraagFitAan(client);
		}
		else
		{
			LOG.warn("Onverwachte ColonKeuze: {}", request.colonKeuze());
		}
	}
}
