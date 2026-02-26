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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.function.UnaryOperator;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.exception.CervixMonsterZoekenExceptie;
import nl.rivm.screenit.main.repository.cervix.CervixMonsterRepository;
import nl.rivm.screenit.main.service.cervix.CervixUitnodigingService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.cervix.Cervix2023StartBepalingService;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.specification.cervix.CervixMonsterSpecification;

import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.Nullable;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@AllArgsConstructor
public class CervixUitnodigingServiceImpl implements CervixUitnodigingService
{

	private final CervixBaseMonsterService monsterService;

	private final ClientService clientService;

	private final Cervix2023StartBepalingService bmkh2023StartBepalingService;

	private final OrganisatieParameterService organisatieParameterService;

	private final CervixMonsterRepository monsterRepository;

	@Override
	public List<CervixUitnodiging> zoekMonsters(Organisatie ingelogdNamensOrganisatie, String monsterId, String bsn, Date geboortedatum, UnaryOperator<String> getString)
		throws CervixMonsterZoekenExceptie
	{
		var aantalIngevuldeVelden = (StringUtils.isBlank(monsterId) ? 0 : 1) + (StringUtils.isBlank(bsn) ? 0 : 1) + (geboortedatum == null ? 0 : 1);
		if (aantalIngevuldeVelden < 2)
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("minstens.2.velden"));
		}

		if (StringUtils.isNotBlank(monsterId))
		{
			var melding = valideerIngevuldMonsterId(monsterId, ingelogdNamensOrganisatie, getString);
			if (melding != null)
			{
				throw new CervixMonsterZoekenExceptie(melding);
			}
		}
		if (StringUtils.isNotBlank(bsn) && !StringUtils.isNumeric(bsn))
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("bsn.geen.nummer"));
		}

		if (StringUtils.isNotBlank(monsterId))
		{
			return zoekUitnodigingOpBasisVanMonsterId(monsterId, bsn, geboortedatum, ingelogdNamensOrganisatie, getString);
		}

		return zoekOpBasisVanBsnEnGeboortedatum(ingelogdNamensOrganisatie, bsn, geboortedatum, getString);
	}

	@Nullable
	private List<CervixUitnodiging> zoekOpBasisVanBsnEnGeboortedatum(Organisatie ingelogdNamensOrganisatie, String bsn, Date geboortedatum, UnaryOperator<String> getString)
		throws CervixMonsterZoekenExceptie
	{
		var client = clientService.getClientByBsn(bsn);
		if (client == null)
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("bsn.onbekend"));
		}
		else if (!client.getPersoon().getGeboortedatum().equals(geboortedatum))
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("geen.met.bsn.en.geboortedatum"));
		}
		return verzamelUitnodigingenUitLaatsteRondeDieHetLabMagZien(ingelogdNamensOrganisatie, client);
	}

	private List<CervixUitnodiging> verzamelUitnodigingenUitLaatsteRondeDieHetLabMagZien(Organisatie ingelogdNamensOrganisatie, Client client)
	{
		var uitnodigingen = new ArrayList<CervixUitnodiging>();
		if (client.getCervixDossier() != null && client.getCervixDossier().getLaatsteScreeningRonde() != null
			&& client.getCervixDossier().getLaatsteScreeningRonde().getUitnodigingen() != null)
		{
			for (var uitnodiging : client.getCervixDossier().getLaatsteScreeningRonde().getUitnodigingen())
			{
				var monster = uitnodiging.getMonster();
				if (monster != null && monsterService.magOrganisatieMonsterInzien(ingelogdNamensOrganisatie, monster))
				{
					uitnodigingen.add(uitnodiging);
				}
			}
		}
		return uitnodigingen;
	}

	private String valideerIngevuldMonsterId(String monsterId, Organisatie ingelogdNamensOrganisatie, UnaryOperator<String> getString)
	{
		if (ingelogdNamensOrganisatie.getOrganisatieType() == OrganisatieType.BMHK_LABORATORIUM)
		{
			var verwachteEersteZASMonsterIdLetter = getVerwachteEersteZASMonsterIdLetter(ingelogdNamensOrganisatie);
			var eersteLetter = monsterId.charAt(0);
			if (eersteLetter == verwachteEersteZASMonsterIdLetter)
			{
				final String monsterNummer = monsterId.substring(1);
				if (!StringUtils.isNumeric(monsterNummer))
				{
					return String.format(getString.apply("nummer.invullen.na.z"), verwachteEersteZASMonsterIdLetter);
				}
			}
			else if (eersteLetter == 'C' || eersteLetter == 'Z')
			{
				return String.format(getString.apply("zas.mag.niet.verwerkt.worden"), ingelogdNamensOrganisatie.getNaam());
			}
			else if (!StringUtils.isNumeric(monsterId))
			{
				return String.format(getString.apply("nummer.invullen"), verwachteEersteZASMonsterIdLetter);
			}
		}
		return null;
	}

	private List<CervixUitnodiging> zoekUitnodigingOpBasisVanMonsterId(String monsterId, String bsn, Date geboortedatum, Organisatie ingelogdNamensOrganisatie,
		UnaryOperator<String> getString) throws CervixMonsterZoekenExceptie
	{
		var monster = monsterService.getMonster(monsterId).orElse(null);
		if (monster == null)
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("geen.met.monster.id"));
		}
		var persoon = monster.getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon();
		if (StringUtils.isNotBlank(bsn) && !bsn.equals(persoon.getBsn()))
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("geen.met.monster.id.en.bsn"));
		}
		if (geboortedatum != null && !geboortedatum.equals(persoon.getGeboortedatum()))
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("geen.met.monster.id.en.geboortedatum"));
		}
		if (!monsterService.magOrganisatieMonsterInzien(ingelogdNamensOrganisatie, monster))
		{
			throw new CervixMonsterZoekenExceptie(getString.apply("laboratorium.mag.monster.niet.inzien"));
		}
		return new ArrayList<>(Collections.singletonList(monster.getUitnodiging()));
	}

	@Override
	public char getVerwachteEersteZASMonsterIdLetter(Organisatie ingelogdNamensOrganisatie)
	{
		boolean nieuweBMHKLabs = organisatieParameterService.getOrganisatieParameter(ingelogdNamensOrganisatie,
			OrganisatieParameterKey.CERVIX_HPV_ORDER_NIEUW, Boolean.FALSE);
		return nieuweBMHKLabs && bmkh2023StartBepalingService.isBmhk2023Actief() ? 'C' : 'Z';
	}

	@Override
	public boolean magMonsterVerwerktWordenDoorLab(Organisatie ingelogdNamensOrganisatie, CervixUitnodiging uitnodiging)
	{
		var verwachteEersteZASMonsterIdLetter = String.valueOf(getVerwachteEersteZASMonsterIdLetter(ingelogdNamensOrganisatie));
		return ingelogdNamensOrganisatie.getOrganisatieType() != OrganisatieType.BMHK_LABORATORIUM ||
			(uitnodiging != null && uitnodiging.getMonster() != null &&
				(uitnodiging.getMonsterType() == CervixMonsterType.UITSTRIJKJE ||
					uitnodiging.getMonster().getMonsterId() != null && uitnodiging.getMonster().getMonsterId().startsWith(verwachteEersteZASMonsterIdLetter)));
	}

	@Override
	public boolean uitnodigingHeeftZasMetNieuweBarcode(CervixUitnodiging uitnodiging)
	{
		return uitnodiging.getMonster() != null && uitnodiging.getMonsterType() == CervixMonsterType.ZAS && uitnodiging.getMonster().getMonsterId() != null
			&& uitnodiging.getMonster().getMonsterId().charAt(0) == 'C';
	}

	@Override
	public boolean magRedenUitnodigingKiezen(CervixScreeningRonde laatsteScreeningRonde)
	{
		return monsterRepository.exists(CervixMonsterSpecification.geefNietIngestuurdeOudeZAS(laatsteScreeningRonde.getDossier()));
	}
}
