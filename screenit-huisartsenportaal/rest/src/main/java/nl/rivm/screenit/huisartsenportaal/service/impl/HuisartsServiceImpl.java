package nl.rivm.screenit.huisartsenportaal.service.impl;

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

import java.util.Date;

import nl.rivm.screenit.huisartsenportaal.dto.HuisartsDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordVergetenDto;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.service.AdresService;
import nl.rivm.screenit.huisartsenportaal.service.AuthenticatieService;
import nl.rivm.screenit.huisartsenportaal.service.HuisartsService;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class HuisartsServiceImpl implements HuisartsService
{

	@Autowired
	private AdresService adresService;

	@Autowired
	private HuisartsRepository huisartsRepository;

	@Autowired
	private ModelMapper modelMapper;

	@Autowired
	private AuthenticatieService authenticatieService;

	@Override
	@Transactional
	public Huisarts updateAndGetHuisarts(HuisartsDto huisartsDto, Huisarts huisarts)
	{
		adresService.updateAndGetAdres(huisartsDto.getPostadres());
		modelMapper.map(huisartsDto, huisarts);

		if (huisartsDto.getUsername() != null && huisartsDto.getWachtwoord() != null && !InlogMethode.USERNAME_PASSWORD.equals(huisarts.getInlogMethode()))
		{
			huisarts.setGebruikersnaam(huisartsDto.getUsername());
			authenticatieService.updateWachtwoord(huisarts, huisartsDto.getWachtwoord());

			var rollen = huisarts.getRollen();
			rollen.remove(Recht.ROLE_REGISTEREN);
			rollen.add(Recht.ROLE_AANVRAGEN);
			huisarts.setActief(true);
		}
		if (Boolean.TRUE.equals(huisartsDto.getOvereenkomst()))
		{
			huisarts.setOvereenkomstGeaccordeerdDatum(new Date());
		}

		huisartsRepository.save(huisarts);
		return huisarts;
	}

	@Override
	@Transactional
	public Huisarts updateAndGetHuisarts(HuisartsDto huisartsDto)
	{
		Huisarts huisarts = null;
		if (huisartsDto.getHuisartsportaalId() != null)
		{
			huisarts = huisartsRepository.findByHuisartsportaalId(huisartsDto.getHuisartsportaalId());
		}
		if (huisarts == null && huisartsDto.getScreenitId() != null)
		{
			huisarts = huisartsRepository.findByScreenitId(huisartsDto.getScreenitId());
		}
		if (huisarts == null)
		{
			huisarts = new Huisarts();
		}
		if (huisarts.getScreenitId() == null)
		{
			huisarts.setScreenitId(huisartsDto.getScreenitId());
		}
		huisarts.setAgbcode(huisartsDto.getAgbcode());
		huisarts.setEmail(huisartsDto.getEmail());
		huisarts.setAanhef(huisartsDto.getAanhef());
		huisarts.setAchternaam(huisartsDto.getAchternaam());
		huisarts.setTussenvoegsel(huisartsDto.getTussenvoegsel());
		huisarts.setVoorletters(huisartsDto.getVoorletters());
		huisarts.setTelefoon(huisartsDto.getTelefoon());
		huisarts.setActief(huisartsDto.getActief());
		huisarts.setExtraEmails(huisartsDto.getExtraEmails());
		huisarts.setAanmeldStatus(AanmeldStatus.valueOf(huisartsDto.getAanmeldStatus()));
		if (huisartsDto.getInlogCode() != null && AanmeldStatus.GEREGISTREERD != huisarts.getAanmeldStatus())
		{
			huisarts.setAttempts(0);
			huisarts.setInlogCode(huisartsDto.getInlogCode());
			huisarts.setInlogMethode(InlogMethode.INLOGCODE);
		}
		huisarts.setInlogCode(huisartsDto.getInlogCode());
		if (huisartsDto.getPostadres() != null)
		{
			huisarts.setPostadres(adresService.updateAndGetAdres(huisartsDto.getPostadres()));
		}

		huisartsRepository.save(huisarts);
		return huisarts;
	}

	@Override
	public Huisarts getHuisartsWith(WachtwoordVergetenDto dto)
	{
		return huisartsRepository.findByEmailAndGebruikersnaam(dto.getEmail(), dto.getGebruikersnaam());
	}

	@Override
	public Huisarts getHuisartsWith(Long screenitId)
	{
		return huisartsRepository.findByScreenitId(screenitId);
	}

}
