package nl.rivm.screenit.main.service.algemeen.impl;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;

import jakarta.persistence.criteria.From;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.main.dto.algemeen.ClientZoekenFilterDto;
import nl.rivm.screenit.main.service.algemeen.ClientZoekenService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.Persoon;
import nl.rivm.screenit.model.Persoon_;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.repository.algemeen.ClientRepository;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.organisatie.model.Adres;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.AdresSpecification.heeftHuisnummer;
import static nl.rivm.screenit.specification.algemeen.AdresSpecification.heeftPostcode;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftANummer;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftBkUitnodigingsnummer;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftBmhkMonsterId;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftBmhkUitnodigingsId;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftDkBarcode;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftDkUitnodigingsId;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftEmailadres;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.heeftNietGbaStatussen;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterGeboortedatum;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.filterTelefoonNummer1;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.heeftBsn;

@Service
@RequiredArgsConstructor
public class ClientZoekenServiceImpl implements ClientZoekenService
{
	private final ClientService clientService;

	private final ClientRepository clientRepository;

	@Override
	public List<Client> zoekClienten(ClientZoekenFilterDto filter)
	{
		var specificaties = new ArrayList<Specification<Client>>();

		if (StringUtils.isNotBlank(filter.getBriefkenmerk()))
		{
			var client = clientService.getClientMetBriefkenmerk(filter.getBriefkenmerk());
			if (client == null)
			{
				return List.of();
			}
			specificaties.add(HibernateObjectSpecification.heeftId(client.getId()));
		}

		if (StringUtils.isNotBlank(filter.getBsn()))
		{
			specificaties.add(heeftBsn(filter.getBsn()).with(Client_.persoon));
		}

		if (StringUtils.isNotBlank(filter.getPostcode()) && filter.getHuisnummer() != null)
		{
			var postcode = filter.getPostcode().replace(" ", "").toUpperCase();
			specificaties.add(heeftPostcode(postcode).with(adresJoin())
				.and(heeftHuisnummer(filter.getHuisnummer()).with(adresJoin())));
		}

		if (StringUtils.isNotBlank(filter.getAnummer()))
		{
			specificaties.add(heeftANummer(filter.getAnummer()));
		}

		if (StringUtils.isNotBlank(filter.getMobielnummer()))
		{
			specificaties.add(filterTelefoonNummer1(filter.getMobielnummer()).with(persoonJoin()));
		}

		if (StringUtils.isNotBlank(filter.getEmailadres()))
		{
			specificaties.add(heeftEmailadres(filter.getEmailadres()));
		}

		if (filter.getBmhkUitnodigingsId() != null)
		{
			specificaties.add(heeftBmhkUitnodigingsId(filter.getBmhkUitnodigingsId()));
		}

		if (filter.getDkUitnodigingsId() != null)
		{
			specificaties.add(heeftDkUitnodigingsId(filter.getDkUitnodigingsId()));
		}

		if (filter.getBkUitnodigingsnummer() != null)
		{
			specificaties.add(heeftBkUitnodigingsnummer(filter.getBkUitnodigingsnummer()));
		}

		if (StringUtils.isNotBlank(filter.getBmhkMonsterId()))
		{
			specificaties.add(heeftBmhkMonsterId(filter.getBmhkMonsterId()));
		}

		if (StringUtils.isNotBlank(filter.getDkBarcode()))
		{
			specificaties.add(heeftDkBarcode(filter.getDkBarcode()));
		}

		if (specificaties.isEmpty())
		{
			return List.of();
		}

		specificaties.add(filterGeboortedatum(DateUtil.toUtilDate(filter.getGeboortedatum())).with(persoonJoin()));

		var statussen = new ArrayList<>(Arrays.asList((GbaStatus.BEZWAAR)));
		if (!zijnGeavanceerdeVeldenGevuld(filter))
		{
			statussen.add(GbaStatus.AFGEVOERD);
		}
		specificaties.add(heeftNietGbaStatussen(statussen));

		return clientRepository.findAll(specificaties.stream().reduce(Specification::and).orElseThrow())
			.stream()
			.sorted(Comparator.comparingInt(client -> client.getGbaStatus() == GbaStatus.AFGEVOERD ? 1 : 0))
			.toList();
	}

	@Override
	public List<Bevolkingsonderzoek> getActieveBvos(Long clientId)
	{
		var client = clientService.getClientById(clientId)
			.orElseThrow();
		var colonDossier = client.getColonDossier();
		var cervixDossier = client.getCervixDossier();
		var mammaDossier = client.getMammaDossier();

		var actieveBvos = new ArrayList<Bevolkingsonderzoek>();
		if (colonDossier != null && !colonDossier.getScreeningRondes().isEmpty())
		{
			actieveBvos.add(Bevolkingsonderzoek.COLON);
		}
		if (cervixDossier != null && !cervixDossier.getScreeningRondes().isEmpty())
		{
			actieveBvos.add(Bevolkingsonderzoek.CERVIX);
		}
		if (mammaDossier != null && !mammaDossier.getScreeningRondes().isEmpty())
		{
			actieveBvos.add(Bevolkingsonderzoek.MAMMA);
		}
		return actieveBvos;
	}

	private static boolean zijnGeavanceerdeVeldenGevuld(ClientZoekenFilterDto filter)
	{
		return StringUtils.isNotBlank(filter.getAnummer())
			|| StringUtils.isNotBlank(filter.getMobielnummer())
			|| StringUtils.isNotBlank(filter.getEmailadres())
			|| filter.getBkUitnodigingsnummer() != null
			|| StringUtils.isNotBlank(filter.getBmhkMonsterId())
			|| filter.getBmhkUitnodigingsId() != null
			|| StringUtils.isNotBlank(filter.getDkBarcode())
			|| filter.getDkUitnodigingsId() != null;
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends Adres>> adresJoin()
	{
		return q ->
		{
			var persoon = join(q, Client_.persoon);
			return join(persoon, Persoon_.gbaAdres);
		};
	}

	private static Function<From<?, ? extends Client>, From<?, ? extends Persoon>> persoonJoin()
	{
		return q -> join(q, Client_.persoon);
	}
}
