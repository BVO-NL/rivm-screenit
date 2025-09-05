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

import java.util.ArrayList;
import java.util.Date;
import java.util.regex.Pattern;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.huisartsenportaal.dto.LoginDto;
import nl.rivm.screenit.huisartsenportaal.dto.RegistrerenDto;
import nl.rivm.screenit.huisartsenportaal.dto.TokenDto;
import nl.rivm.screenit.huisartsenportaal.dto.WachtwoordAanvragenDto;
import nl.rivm.screenit.huisartsenportaal.exception.UserNotFoundException;
import nl.rivm.screenit.huisartsenportaal.model.Huisarts;
import nl.rivm.screenit.huisartsenportaal.model.Medewerker;
import nl.rivm.screenit.huisartsenportaal.model.enums.AanmeldStatus;
import nl.rivm.screenit.huisartsenportaal.model.enums.InlogMethode;
import nl.rivm.screenit.huisartsenportaal.model.enums.Recht;
import nl.rivm.screenit.huisartsenportaal.model.enums.Scope;
import nl.rivm.screenit.huisartsenportaal.repository.HuisartsRepository;
import nl.rivm.screenit.huisartsenportaal.service.AuthenticatieService;
import nl.rivm.screenit.huisartsenportaal.service.JwtService;
import nl.rivm.screenit.huisartsenportaal.util.CodeGenerator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@AllArgsConstructor
public class AuthenticatieServiceImpl implements AuthenticatieService
{
	private final HuisartsRepository huisartsRepository;

	private final AuthenticationManager authenticationManager;

	private final PasswordEncoder passwordEncoder;

	public static final Pattern VALID_EMAIL_ADDRESS_REGEX = Pattern.compile("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$", Pattern.CASE_INSENSITIVE);

	@Autowired
	private JwtService jwtService;

	@Override
	public String getScope()
	{
		return Scope.LOGIN;
	}

	@Override
	public TokenDto registreren(RegistrerenDto registrerenDto)
	{
		var huisarts = huisartsRepository.findByAgbcodeAndInlogCodeAndInlogMethode(registrerenDto.getAgbCode(), registrerenDto.getRegistratieCode(), InlogMethode.INLOGCODE);
		if (huisarts == null)
		{
			throw new UserNotFoundException("Medewerker niet gevonden.");
		}

		return getToken(huisarts, Scope.REGISTREREN);
	}

	@Override
	public TokenDto wachtwoordAanvragen(WachtwoordAanvragenDto wachtwoordAanvragenDto)
	{
		var huisarts = huisartsRepository.findByGebruikersnaamAndInlogCodeAndInlogMethode(wachtwoordAanvragenDto.getEmailOfGebruikersnaam(),
			wachtwoordAanvragenDto.getInlogCode(),
			InlogMethode.USERNAME_PASSWORD);
		if (huisarts == null && VALID_EMAIL_ADDRESS_REGEX.matcher(wachtwoordAanvragenDto.getEmailOfGebruikersnaam()).find())
		{
			huisarts = huisartsRepository.findByEmailAndInlogCodeAndInlogMethode(wachtwoordAanvragenDto.getEmailOfGebruikersnaam(),
				wachtwoordAanvragenDto.getInlogCode(),
				InlogMethode.USERNAME_PASSWORD);
		}
		if (huisarts == null)
		{
			throw new BadCredentialsException("Medewerker niet gevonden.");
		}
		huisarts.setAanmeldStatus(AanmeldStatus.WACHTWOORD_RESET);
		huisartsRepository.save(huisarts);

		return getToken(huisarts, Scope.WACHTWOORDVERGETEN);
	}

	@Override
	public Huisarts updateWachtwoord(Huisarts huisarts, String wachtwoord)
	{
		huisarts.setPassword(passwordEncoder.encode(wachtwoord));
		huisarts.setInlogMethode(InlogMethode.USERNAME_PASSWORD);
		huisarts.setAanmeldStatus(AanmeldStatus.GEREGISTREERD);
		huisarts.getRollen().remove(Recht.ROLE_REGISTEREN);
		if (!huisarts.getRollen().contains(Recht.ROLE_AANVRAGEN))
		{
			huisarts.getRollen().add(Recht.ROLE_AANVRAGEN);
		}
		huisarts.setInlogCode(null);
		huisartsRepository.save(huisarts);
		return huisarts;
	}

	@Override
	public TokenDto inloggen(LoginDto loginDto)
	{
		try
		{
			var auth = authenticationManager.authenticate(
				new UsernamePasswordAuthenticationToken(
					loginDto.getGebruikersnaam(),
					loginDto.getWachtwoord()
				)
			);
			var huisarts = (Huisarts) auth.getPrincipal();
			resetAttempts(huisarts);
			return getToken(huisarts, Scope.LOGIN);
		}
		catch (BadCredentialsException e)
		{
			var huisarts = huisartsRepository.findByGebruikersnaam(loginDto.getGebruikersnaam());
			if (huisarts != null)
			{
				var attempts = incrementAttempts(huisarts);
				throw new BadCredentialsException("Uw ingevoerde wachtwoord is ongeldig. U heeft nog " + attempts + " pogingen.");
			}
			throw new BadCredentialsException("Inloggen mislukt. Medewerker niet gevonden.");
		}
	}

	@Override
	public boolean controleerWachtwoord(String plainWachtwoord, String encodedWachtwoord)
	{
		return passwordEncoder.matches(plainWachtwoord, encodedWachtwoord);
	}

	@Override
	@Transactional
	public Huisarts wachtwoordVergeten(Huisarts huisarts) throws IllegalStateException
	{
		if (huisarts == null)
		{
			throw new IllegalStateException("Er is geen huisarts gevonden met deze gebruikersnaam en e-mail");
		}

		if (InlogMethode.INLOGCODE.equals(huisarts.getInlogMethode()))
		{
			throw new IllegalStateException("U zult zich eerst moeten registreren voordat u een wachtwoord kan aanvragen");
		}

		if (InlogMethode.USERNAME_PASSWORD.equals(huisarts.getInlogMethode()))
		{
			var codeB = CodeGenerator.genereerCode(3, 3);
			huisarts.setInlogCode(codeB);
			huisarts.setPassword(null);
			var rechten = new ArrayList<Recht>();
			rechten.add(Recht.ROLE_REGISTEREN);
			huisarts.setRollen(rechten);
			huisarts.setAttempts(0);
			huisartsRepository.save(huisarts);
		}
		return huisarts;
	}

	@Override
	public Integer incrementAttempts(Huisarts huisarts)
	{
		if (huisarts == null)
		{
			return 0;
		}
		var attempts = huisarts.getAttempts();
		if (attempts == Medewerker.MAX_ATTEMPS)
		{
			attempts = 0;
		}
		huisarts.setAttempts(++attempts);
		huisarts.setLastAttemptDate(new Date());
		huisartsRepository.save(huisarts);
		return Medewerker.MAX_ATTEMPS - attempts + 1; 
	}

	@Override
	public void resetAttempts(Huisarts huisarts)
	{
		huisarts.setAttempts(0);
		huisarts.setLastAttemptDate(new Date());
		huisartsRepository.save(huisarts);
	}

	private TokenDto getToken(Huisarts huisarts, String scope)
	{
		var token = new TokenDto();
		token.setToken(jwtService.generateToken(huisarts));
		token.setScope(scope);
		return token;
	}
}
