package nl.rivm.screenit.mamma.se.proxy.controller;

/*-
 * ========================LICENSE_START=================================
 * se-proxy
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

import java.io.IOException;

import jakarta.servlet.http.HttpServletRequest;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.mamma.se.proxy.SeProxyApplication;
import nl.rivm.screenit.mamma.se.proxy.model.AutorisatieDto;
import nl.rivm.screenit.mamma.se.proxy.model.LoginContext;
import nl.rivm.screenit.mamma.se.proxy.model.LogischeSessie;
import nl.rivm.screenit.mamma.se.proxy.model.NavigatieDto;
import nl.rivm.screenit.mamma.se.proxy.services.AchtergrondRequestService;
import nl.rivm.screenit.mamma.se.proxy.services.AuthenticatieService;
import nl.rivm.screenit.mamma.se.proxy.services.AutorisatieService;
import nl.rivm.screenit.mamma.se.proxy.services.ConfiguratieService;
import nl.rivm.screenit.mamma.se.proxy.services.GebruikerStoreService;
import nl.rivm.screenit.mamma.se.proxy.services.LogischeSessieService;
import nl.rivm.screenit.mamma.se.proxy.services.NfcOtpAdministratieService;
import nl.rivm.screenit.mamma.se.proxy.services.ProxyService;
import nl.rivm.screenit.mamma.se.proxy.services.SeRestSocketService;
import nl.rivm.screenit.mamma.se.proxy.services.SeStatusService;
import nl.rivm.screenit.mamma.se.proxy.services.TransactionQueueService;
import nl.rivm.screenit.mamma.se.proxy.services.WebSocketProxyService;
import nl.rivm.screenit.mamma.se.proxy.util.Constants;
import nl.rivm.screenit.mamma.se.proxy.util.DateUtil;
import nl.rivm.screenit.mamma.se.proxy.util.SafeStringUtil;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RestController
@RequestMapping("/api/authenticatie")
@RequiredArgsConstructor
public class AuthenticatieProxyController
{
	private final ObjectMapper objectMapper = new ObjectMapper();

	private static final Logger LOG = LoggerFactory.getLogger(AuthenticatieProxyController.class);

	private static final boolean GENEREER_LOGGING = true;

	private static final boolean GENEREER_GEEN_LOGGING = false;

	private final ProxyService proxyService;

	private final LogischeSessieService logischeSessieService;

	private final GebruikerStoreService gebruikerStoreService;

	private final NfcOtpAdministratieService nfcOtpAdministratieService;

	private final SeRestSocketService seRestSocketService;

	private final ConfiguratieService configuratieService;

	private final AchtergrondRequestService achtergrondRequestService;

	private final SeStatusService seStatusService;

	private final WebSocketProxyService webSocketProxyService;

	private final TransactionQueueService transactionQueueService;

	private final AuthenticatieService authenticatieService;

	private final AutorisatieService autorisatieService;

	@Value("${DISABLE_NFC_AUTHENTICATION:#{false}}")
	private final boolean disableNFCAuthentication;

	@PostMapping("/inloggen")
	public ResponseEntity<String> login(@RequestHeader(value = "Authorization") String credentials, @RequestHeader String yubikeyIdentificatie,
		@RequestHeader String nfcServerVersie,
		@RequestHeader(value = "Yubikey") String yubikey, HttpServletRequest request) throws JsonProcessingException
	{
		seStatusService.setProxyIpFromClientIpIfSeCodeIsMissing(request.getRemoteAddr());
		ResponseEntity<String> response;
		if (disableNFCAuthentication)
		{
			var logischeSessie = new LogischeSessie(credentials, Constants.GEEN_IDENTIFICATIE);
			response = login(logischeSessie, Constants.GEEN_OTP, null, GENEREER_LOGGING, nfcServerVersie);
		}
		else if (yubikeyIdentificatie.equals(Constants.GEEN_IDENTIFICATIE) || yubikey.equals(Constants.GEEN_OTP))
		{
			LOG.info("Inlogpoging zonder volledige Yubikey credentials: {}, {}", yubikeyIdentificatie, yubikey);
			response = ResponseEntity.status(HttpStatus.BAD_REQUEST).body(responseJson("Geen Yubikey gedetecteerd.", "YUBIKEY_AFWEZIG"));
		}
		else if (credentials.equals(Constants.EMPTY_CREDENTIALS))
		{
			response = meenemenLogischeSessie(yubikeyIdentificatie, yubikey);
		}
		else if (nfcOtpAdministratieService.zelfdeOtpAlsLaatsteSuccesvolle(yubikeyIdentificatie, yubikey))
		{
			LOG.info("Inlogpoging met zelfde OTP als laatste succesvolle login: {}", yubikeyIdentificatie);
			response = ResponseEntity.status(HttpStatus.FORBIDDEN).body(responseJson("Leg Yubikey opnieuw op de lezer en log nog een keer in", "YUIBIKEY_ZELFDE_OTP"));
		}
		else
		{
			var logischeSessie = new LogischeSessie(credentials, yubikeyIdentificatie);
			response = login(logischeSessie, yubikey, null, GENEREER_LOGGING, nfcServerVersie);
			if (response.getStatusCode().equals(HttpStatus.OK))
			{
				logischeSessieService.addLogischeSessie(logischeSessie);
			}
		}

		webSocketProxyService.broadCastTijdUpdateNaarWerkstations("Login");
		return response;
	}

	private ResponseEntity<String> meenemenLogischeSessie(String yubikeyIdentificatie, String yubikey) throws JsonProcessingException
	{
		ResponseEntity<String> response;
		var logischeSessie = logischeSessieService.getLogischeSessieMetIdentificatie(yubikeyIdentificatie);
		if (logischeSessie == null)
		{
			LOG.warn("INLOGGEN (sessie meenemen) met identificatie: {} -> Logische sessie niet gevonden, gebruiker niet geautoriseerd.", yubikeyIdentificatie);
			response = ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
		}
		else if (!seStatusService.isOnline())
		{
			LOG.info("INLOGGEN (sessie meenemen) met identificatie: {} -> SE is offline, haal huidige logische sessie op.", yubikeyIdentificatie);
			response = logischeSessie.getLoginAntwoord();
		}
		else if (nfcOtpAdministratieService.zelfdeOtpAlsLaatsteSuccesvolle(yubikeyIdentificatie, yubikey))
		{
			LOG.info("INLOGGEN (sessie meenemen) met identificatie: {} -> Zelfde OTP, skip inlog op centraal", yubikeyIdentificatie);
			broadcastOnlineStatus();
			response = logischeSessie.getLoginAntwoord();
		}
		else
		{
			var huidigLoginAntwoord = logischeSessie.getLoginAntwoord();
			LOG.info("INLOGGEN (sessie meenemen) met identificatie: {} -> SE is online, controleer Yubikey op centraal.", yubikeyIdentificatie);
			response = login(logischeSessie, yubikey, logischeSessie.getNavigatie(), GENEREER_GEEN_LOGGING, "");
			if (response.getStatusCode() != HttpStatus.OK && response.getStatusCode() != HttpStatus.FORBIDDEN)
			{
				logischeSessie.setLoginAntwoord(huidigLoginAntwoord);
				seRestSocketService.setSeVerbindingStatus(false);
				LOG.info("INLOGGEN (sessie meenemen) met identificatie: {} -> Sessie meenemen ging fout, Error code {}. Haal huidige sessie op", yubikeyIdentificatie,
					response.getStatusCode());
				response = logischeSessie.getLoginAntwoord();
			}
		}
		return response;
	}

	private ResponseEntity<String> login(LogischeSessie logischeSessie, String yubikey, NavigatieDto navigatie, boolean genereerLogging,
		String nfcServerVersie)
		throws JsonProcessingException
	{
		if (seStatusService.isOnline() || seStatusService.getSeCode() == null)
		{
			return onlineInloggen(logischeSessie, yubikey, navigatie, genereerLogging, nfcServerVersie);
		}
		else
		{
			LOG.info("INLOGGEN met identificatie: {} -> SE is offline, probeer offline in te loggen.", logischeSessie.getYubikeyIdentificatie());
			return offlineInloggen(logischeSessie);
		}
	}

	private ResponseEntity<String> onlineInloggen(LogischeSessie logischeSessie, String yubikey, NavigatieDto navigatie, boolean genereerLogging, String nfcServerVersie)
		throws JsonProcessingException
	{
		logischeSessie.setLaatsteUpdate(DateUtil.getCurrentDateTime());
		var pathPostfix = genereerLogging ? "/authenticatie/inloggen/logging" : "/authenticatie/inloggen/geen_logging";
		var requestBuilder = proxyService.getProxyRequestEntity(pathPostfix, HttpMethod.POST);
		requestBuilder.header("Authorization", logischeSessie.getCredentials());
		requestBuilder.header("nfcServerVersie", nfcServerVersie);
		requestBuilder.header("yubikey", yubikey);
		requestBuilder.header("navigatie", navigatie == null ? "{}" : objectMapper.writeValueAsString(navigatie).replace("navigatieType", "type"));
		var responseEntity = proxyService.sendUncheckedProxyRequest(requestBuilder.build(), String.class);
		logischeSessie.setLoginAntwoord(responseEntity);
		responseEntity = verwerkLoginResponse(true, logischeSessie);
		updateLaatstSuccesvolleOtp(yubikey, responseEntity.getStatusCode(), logischeSessie.getYubikeyIdentificatie());
		seRestSocketService.verversPingEnPongConfig();

		return responseEntity;
	}

	private ResponseEntity<String> offlineInloggen(LogischeSessie logischeSessie)
	{
		var loginContext = createLoginContext(logischeSessie, authenticatieService.getAccountIdFromUsername(logischeSessie.getGebruikersnaam()));
		if (loginContext.getAccountId() != null)
		{
			var ingelogdeGebruikerDto = authenticatieService.getIngelogdeGebruiker(loginContext);

			if (ingelogdeGebruikerDto != null)
			{
				try
				{
					if (autorisatieService.isGeautoriseerdVoorInloggen(ingelogdeGebruikerDto))
					{
						logischeSessie.setLaatsteUpdate(DateUtil.getCurrentDateTime());
						ingelogdeGebruikerDto.setLaatsteInlog(DateUtil.getCurrentDateTime().toLocalDate());
						authenticatieService.updateIngelogdeGebruiker(ingelogdeGebruikerDto);
						var responseEntity = ResponseEntity.ok(ingelogdeGebruikerDto.getLoginResponse());
						logischeSessie.setLoginAntwoord(responseEntity);
						return verwerkLoginResponse(false, logischeSessie);
					}
				}
				catch (IOException e)
				{
					LOG.error("Foutief jsonObject na offline login.", e);
					return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
				}
			}
			else
			{
				return ResponseEntity.status(HttpStatus.FORBIDDEN).body("{\"message\":\"Aanmelden mislukt tijdens SE offline\"}");
			}
		}

		return gatewayTimeoutResponse();
	}

	private ResponseEntity<String> gatewayTimeoutResponse()
	{
		return ResponseEntity.status(HttpStatus.GATEWAY_TIMEOUT).build();
	}

	private void updateLaatstSuccesvolleOtp(String yubikey, HttpStatusCode statusCode, String yubikeyIdentificatie)
	{
		if (statusCode.equals(HttpStatus.OK) && !Constants.GEEN_IDENTIFICATIE.equals(yubikeyIdentificatie) && StringUtils.isNoneEmpty(yubikey))
		{
			nfcOtpAdministratieService.setLaatsteSuccesvolleOtp(yubikeyIdentificatie, yubikey);
		}
	}

	private ResponseEntity<String> verwerkLoginResponse(boolean vanCentraal, LogischeSessie logischeSessie)
	{
		var response = logischeSessie.getLoginAntwoord();

		if (response.getStatusCode().equals(HttpStatus.OK))
		{
			try
			{
				var autorisatieDto = objectMapper.readValue(response.getBody(), AutorisatieDto.class);
				var instellingGebruikerId = autorisatieDto.getInstellingGebruikerId();
				var displayName = autorisatieDto.getDisplayName();
				var seCode = autorisatieDto.getSeCode();

				if (seStatusService.getSeCode() == null)
				{
					seStatusService.setSeCode(seCode);
				}

				if (vanCentraal)
				{
					response = verwerkParametersVanCentraal(autorisatieDto);
					var loginContext = createLoginContext(logischeSessie, instellingGebruikerId);
					authenticatieService.administreerOnlineInlog(loginContext, response.getBody());
				}
				broadcastOnlineStatus();
				webSocketProxyService.broadcast(gebruikerStoreService.addGebruiker(instellingGebruikerId, displayName));
			}
			catch (IOException e)
			{
				LOG.error("Foutief jsonObject terug van ScreenIT centraal na login, mogelijk is IP-adres van SE niet goed ingesteld in centraal.");
			}
			achtergrondRequestService.ensureRunning();
			transactionQueueService.ensureRunningQueueVerwerking();
		}
		return response;
	}

	private ResponseEntity<String> verwerkParametersVanCentraal(AutorisatieDto autorisatieDto) throws JsonProcessingException
	{
		var seParameters = autorisatieDto.getSeParameters();
		configuratieService.insertOrUpdateConfiguratieValues(seParameters);

		seParameters.clear(); 

		var newResponseBody = objectMapper.writeValueAsString(autorisatieDto);
		return ResponseEntity.status(HttpStatus.OK).contentType(MediaType.APPLICATION_JSON).body(newResponseBody);
	}

	private void broadcastOnlineStatus()
	{
		webSocketProxyService.broadcast(seStatusService.isOnline() ? "ONLINE" : "OFFLINE");
	}

	@PostMapping("/identificeren")
	public ResponseEntity<String> identificeer(@RequestBody String yubikeyIdentificatie)
	{
		var logischeSessie = logischeSessieService.getLogischeSessieMetIdentificatie(yubikeyIdentificatie);
		var safeYubikeyIdentificatie = SafeStringUtil.maakStringMetUserInputVeiligVoorLogging(yubikeyIdentificatie);
		if (logischeSessie == null)
		{
			if (yubikeyIdentificatie.equals(Constants.GEEN_IDENTIFICATIE) || yubikeyIdentificatie.equals(Constants.GEEN_OTP))
			{
				LOG.info("Identificeerpoging zonder Yubikey identificatie: {}", safeYubikeyIdentificatie);
				return ResponseEntity.status(HttpStatus.OK).body(responseJson("Geen Yubikey gedetecteerd", "YUBIKEY_AFWEZIG"));
			}
			else
			{
				return ResponseEntity.status(HttpStatus.OK).body(responseJson("Onbekende Yubikey gedetecteerd", "YUBIKEY_ONBEKEND"));
			}
		}

		if (logischeSessieService.isVerlopen(logischeSessie))
		{
			logischeSessieService.verwijderLogischeSessie(logischeSessie);
			LOG.info("Identificeerpoging met verlopen logische sessie: {}", safeYubikeyIdentificatie);
			return ResponseEntity.status(HttpStatus.OK).body(responseJson("Verlopen sessie", "YUBIKEY_VERLOPEN"));
		}

		LOG.info("Er bestaat een logische sessie voor aangeboden Yubikey {}, geef positief signaal terug aan client.", safeYubikeyIdentificatie);
		return ResponseEntity.status(HttpStatus.OK).body(responseJson("Yubikey herkend", "YUBIKEY_HERKEND"));
	}

	@GetMapping("/isNfcEnabled")
	public ResponseEntity<String> isNfcEnabled()
	{
		return ResponseEntity.status(HttpStatus.OK).header("NFC_ENABLED", SeProxyApplication.getEnvironmentInfo().isNfcEnabled().toString()).build();
	}

	@PostMapping("/uitloggen")
	public ResponseEntity<String> logout(@RequestBody String yubikeyIdentificatie, @RequestHeader("accountId") String accountId)
	{
		var safeYubikeyIdentificatie = SafeStringUtil.maakStringMetUserInputVeiligVoorLogging(yubikeyIdentificatie);
		LOG.debug("Identification:\t {}", safeYubikeyIdentificatie);

		var logischeSessie = logischeSessieService.getLogischeSessieMetIdentificatie(yubikeyIdentificatie);
		logischeSessieService.verwijderLogischeSessie(logischeSessie);

		if (seStatusService.isOnline())
		{
			var requestBuilder = proxyService.getProxyRequestEntityAccount("/authenticatie/uitloggen", HttpMethod.POST, accountId);
			return proxyService.sendUncheckedProxyRequest(requestBuilder.build(), String.class);
		}
		else
		{
			return ResponseEntity.ok("true");
		}
	}

	private String responseJson(String message, String code)
	{
		var node = objectMapper.createObjectNode();
		node.put("message", message);
		node.put("code", code);
		return node.toString();
	}

	private LoginContext createLoginContext(LogischeSessie logischeSessie, Long accountId)
	{
		var loginContext = new LoginContext();
		loginContext.setGebruikersnaam(logischeSessie.getGebruikersnaam());
		loginContext.setPlainWachtwoord(logischeSessie.getWachtWoord());

		var yubikeyIdentificatie = logischeSessie.getYubikeyIdentificatie();
		if (Constants.GEEN_IDENTIFICATIE.equals(yubikeyIdentificatie))
		{
			yubikeyIdentificatie += "-" + accountId;
		}

		loginContext.setYubikeyIdentificatie(yubikeyIdentificatie);

		if (accountId != null)
		{
			loginContext.setEncryptedWachtwoord(authenticatieService.hashWachtwoord(logischeSessie.getWachtWoord(), accountId));
			loginContext.setAccountId(accountId);
		}

		return loginContext;
	}
}
