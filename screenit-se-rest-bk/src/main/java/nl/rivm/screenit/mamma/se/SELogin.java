package nl.rivm.screenit.mamma.se;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Locale;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.se.dto.LoginDto;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.mamma.se.service.SELogService;
import nl.rivm.screenit.mamma.se.service.SeAutorisatieService;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.security.OrganisatieMedewerkerToken;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.BaseMedewerkerService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.yubikey.shiro.YubikeyToken;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.IncorrectCredentialsException;
import org.apache.shiro.authc.UnknownAccountException;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.mgt.SecurityManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SELogin
{
	private static final Logger LOG = LoggerFactory.getLogger(SELogin.class);

	private static final String DEFAULT_MESSAGE = "Aanmelden mislukt, mogelijk zijn de ingevoerde gegevens niet correct.";

	private static final String GEEN_OTP = "GEEN_OTP";

	private final SimplePreferenceService preferenceService;

	private final AuthenticatieService authenticatieService;

	private final SeAutorisatieService seAutorisatieService;

	private final MammaScreeningsEenheidService screeningseenhedenService;

	private final HibernateService hibernateService;

	private final BaseMedewerkerService medewerkerService;

	private final SELogService logService;

	private final String applicationEnvironment;

	private final ICurrentDateSupplier currentDateSupplier;

	private Long screeningsEenheidId;

	private Long accountId;

	public SELogin()
	{
		preferenceService = ApplicationContextProvider.getApplicationContext().getBean(SimplePreferenceService.class);
		authenticatieService = ApplicationContextProvider.getApplicationContext().getBean(AuthenticatieService.class);
		seAutorisatieService = ApplicationContextProvider.getApplicationContext().getBean(SeAutorisatieService.class);
		screeningseenhedenService = ApplicationContextProvider.getApplicationContext().getBean(MammaScreeningsEenheidService.class);
		hibernateService = ApplicationContextProvider.getApplicationContext().getBean(HibernateService.class);
		medewerkerService = ApplicationContextProvider.getApplicationContext().getBean(BaseMedewerkerService.class);
		logService = ApplicationContextProvider.getApplicationContext().getBean(SELogService.class);
		applicationEnvironment = ApplicationContextProvider.getApplicationContext().getBean("applicationEnvironment", String.class);
		currentDateSupplier = ApplicationContextProvider.getApplicationContext().getBean(ICurrentDateSupplier.class);
		Locale.setDefault(Constants.LOCALE_NL);
	}

	public LoginDto doLogin(String seCode, LocalDateTime proxyDatumTijd, String gebruikersnaam, String plainWachtwoord, String yubikey, String versie, String nfcServerVersie,
		boolean genereerLoggebeurtenis)
	{
		SecurityManager securityManager = SecurityUtils.getSecurityManager();
		LoginDto result = new LoginDto(false);

		MammaScreeningsEenheid screeningsEenheid = screeningseenhedenService.getActieveScreeningsEenheidByCode(seCode);
		if (screeningsEenheid == null)
		{
			LOG.error("Screeningseenheid niet herkend! SE-code: " + seCode);
			result.setMessage(DEFAULT_MESSAGE);
			return result;
		}
		screeningsEenheidId = screeningsEenheid.getId();
		Medewerker medewerker = null;

		UsernamePasswordToken token = yubikey.equals(GEEN_OTP) ? new UsernamePasswordToken(gebruikersnaam, plainWachtwoord)
			: new YubikeyToken(gebruikersnaam, plainWachtwoord, yubikey);

		try
		{
			medewerker = medewerkerService.getMedewerkerByGebruikersnaam(token.getUsername()).orElse(null);

			if (medewerker == null)
			{
				logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, null, screeningsEenheid, proxyDatumTijd, "Onbekende gebruikersnaam");
				result.setMessage(DEFAULT_MESSAGE);
			}
			else if (token instanceof YubikeyToken && medewerker.getInlogMethode() != InlogMethode.YUBIKEY)
			{
				LOG.error("Medewerker probeert in te loggen met Yubikey, maar inlogmethode is: {}", medewerker.getInlogMethode());
				result.setMessage(DEFAULT_MESSAGE);
			}
			else if (!(token instanceof YubikeyToken) && medewerker.getInlogMethode() != InlogMethode.GEBRUIKERSNAAM_WACHTWOORD)
			{
				LOG.error("Medewerker probeert in te loggen met Gebruikersnaam/ww, maar inlogmethode is: {}", medewerker.getInlogMethode());
				result.setMessage(DEFAULT_MESSAGE);
			}
			else if (!(token instanceof YubikeyToken) && applicationEnvironment.equalsIgnoreCase("PRODUCTIE"))
			{
				LOG.error("Medewerker probeert in te loggen met Gebruikersnaam/ww in productie, inlogmethode bij medewerker: {}", medewerker.getInlogMethode());
				result.setMessage(DEFAULT_MESSAGE);
			}
			else
			{
				securityManager.authenticate(token);
				String meldingNavActiefVanafEnTotEnMet = MedewerkerUtil.meldingNavActiefVanafEnTotEnMet(medewerker, currentDateSupplier.getDate());
				if (meldingNavActiefVanafEnTotEnMet != null)
				{
					result.setMessage(meldingNavActiefVanafEnTotEnMet);
				}
				else
				{
					result = login(screeningsEenheid, proxyDatumTijd, medewerker, versie, nfcServerVersie, genereerLoggebeurtenis);
				}
			}
		}
		catch (UnknownAccountException | IncorrectCredentialsException ice)
		{
			authenticatieService.foutieveInlogpoging(medewerker);
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, medewerker, screeningsEenheid, proxyDatumTijd,
				"Verkeerde credentials of herhalend gebruik van dezelfde Yubikey code");
			result.setMessage(getError(medewerker, screeningsEenheid, proxyDatumTijd));
		}
		catch (AuthenticationException authExcept)
		{
			LOG.error("Onbekende fout bij inloggen (is het wachtwoord ingevuld?)", authExcept);
		}

		return result;
	}

	private LoginDto login(MammaScreeningsEenheid screeningsEenheid, LocalDateTime proxyDatumTijd, Medewerker medewerker, String versie, String nfcServerVersie,
		boolean genereerLoggebeurtenis)
	{
		LoginDto result = new LoginDto(false);

		if (!authenticatieService.isAccountLocked(medewerker))
		{
			authenticatieService.unlockAccount(medewerker);

			List<OrganisatieMedewerker> organisatieMedewerkers = authenticatieService.getActieveOrganisatieMedewerkers(medewerker);
			if (organisatieMedewerkers.size() == 0)
			{

				logGeenActieveOrganisatie(medewerker, screeningsEenheid, proxyDatumTijd);
				result.setMessage("Er zijn geen actieve organisaties gekoppeld aan deze medewerker.");
				LOG.error(result.getMessage());
			}
			else
			{
				OrganisatieMedewerker inlogOrganisatieMedewerker = getOrganisatieMedewerker(organisatieMedewerkers, screeningsEenheid);
				if (inlogOrganisatieMedewerker != null)
				{

					OrganisatieMedewerkerToken token = new OrganisatieMedewerkerToken(inlogOrganisatieMedewerker.getId());
					SecurityUtils.getSubject().login(token);
					if (seAutorisatieService.isGeautoriseerdVoorInloggen(inlogOrganisatieMedewerker.getId()))
					{
						accountId = inlogOrganisatieMedewerker.getId();
						result.setSuccess(true);
						String seVersie = versie == null ? "onbekend" : versie;
						String nfcVersie = nfcServerVersie == null || nfcServerVersie.equals("undefined") ? "onbekend" : nfcServerVersie;
						String logBericht = String.format("SE-Proxy versie: %s, Nfc webserver versie: %s", seVersie, nfcVersie);
						if (genereerLoggebeurtenis)
						{
							logService.logInfo(LogGebeurtenis.INLOGGEN, inlogOrganisatieMedewerker, screeningsEenheid, proxyDatumTijd, logBericht);
						}
						else
						{
							LOG.info("Sessie meenemen {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inlogOrganisatieMedewerker), logBericht);
						}
					}
					else
					{
						logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inlogOrganisatieMedewerker, screeningsEenheid, proxyDatumTijd,
							"Medewerker heeft niet het Inschrijven op SE recht om in te loggen.");
						SecurityUtils.getSubject().logout();
						result.setSuccess(false);
						result.setMessage("Inloggen is mislukt. Het recht Inschrijven op SE ontbreekt.");
					}
				}
				else
				{
					logGeenActieveOrganisatie(medewerker, screeningsEenheid, proxyDatumTijd);
					result.setMessage("Inloggen mislukt. Medewerker is niet gekoppeld aan de Screeningsorganisatie "
						+ screeningsEenheid.getBeoordelingsEenheid().getParent().getRegio().getNaam());
				}
			}
		}
		else
		{
			result.setMessage(getError(medewerker, screeningsEenheid, proxyDatumTijd));
		}
		return result;
	}

	private String seLogCode(MammaScreeningsEenheid screeningsEenheid)
	{
		return screeningsEenheid == null ? "SE-???" : screeningsEenheid.getCode();
	}

	private void logGeenActieveOrganisatie(Medewerker medewerker, MammaScreeningsEenheid screeningsEenheid, LocalDateTime proxyDatumTijd)
	{
		logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, medewerker, screeningsEenheid, proxyDatumTijd,
			"Gebruikersnaam: " + medewerker.getGebruikersnaam()
				+ ", geen actieve organisatie gekoppeld aan medewerker of niet voldoende rechten voor gekoppelde organisatie(s).");
	}

	private OrganisatieMedewerker getOrganisatieMedewerker(List<OrganisatieMedewerker> organisatieMedewerkers, MammaScreeningsEenheid inlogScreeningsEenheid)
	{
		OrganisatieMedewerker inlogOrganisatieMedewerker = null;
		for (OrganisatieMedewerker organisatieMedewerker : organisatieMedewerkers)
		{

			Organisatie organisatie = organisatieMedewerker.getOrganisatie(); 
			if (organisatie instanceof ScreeningOrganisatie)
			{
				BeoordelingsEenheid be = inlogScreeningsEenheid.getBeoordelingsEenheid();
				Organisatie ce = be.getParent();
				Organisatie so = ce.getRegio();
				if (so.equals(organisatie))
				{
					inlogOrganisatieMedewerker = organisatieMedewerker;
				}
			}
		}
		return inlogOrganisatieMedewerker;
	}

	private String getError(Medewerker inTeLoggenMedewerker, MammaScreeningsEenheid screeningsEenheid, LocalDateTime proxyDatumTijd)
	{
		Integer foutieveAanmeldpogingenTimeout = preferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
		String resultMessage = "";
		if (foutieveAanmeldpogingenTimeout == null)
		{
			foutieveAanmeldpogingenTimeout = 30;
		}

		if (Boolean.FALSE.equals(inTeLoggenMedewerker.getActief()))
		{
			resultMessage = "Uw account is gedeactiveerd. Neem contact op met uw beheerder";
			LOG.error("SE: {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inTeLoggenMedewerker), resultMessage);
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, screeningsEenheid, proxyDatumTijd,
				"Inlogpoging met een gedeactiveerd account");
		}
		else if (authenticatieService.isAccountLocked(inTeLoggenMedewerker))
		{
			if (InlogStatus.GEBLOKKEERD.equals(inTeLoggenMedewerker.getInlogstatus()))
			{
				resultMessage = "Uw account is geblokkeerd. Neem contact op met uw beheerder";
			}
			else if (InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(inTeLoggenMedewerker.getInlogstatus()))
			{
				resultMessage = "Uw account is voor " + foutieveAanmeldpogingenTimeout + " minuten geblokkeerd";
			}
			LOG.error("SE: {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inTeLoggenMedewerker), resultMessage);
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, screeningsEenheid, proxyDatumTijd,
				"Inlogpoging met een geblokkeerd account");
		}
		else if (inTeLoggenMedewerker != null && InlogStatus.OK.equals(inTeLoggenMedewerker.getInlogstatus())
			&& inTeLoggenMedewerker.getFoutieveInlogpogingen() != null)
		{
			Integer maxFoutieveAanmeldpogingen = preferenceService.getInteger(PreferenceKey.MAXIMUM_FOUTIEVE_AANMELDPOGINGEN.name());
			if (maxFoutieveAanmeldpogingen == null)
			{
				maxFoutieveAanmeldpogingen = 3;
			}
			int aantalPogingenResterend = maxFoutieveAanmeldpogingen - inTeLoggenMedewerker.getFoutieveInlogpogingen();
			resultMessage = "Aanmelden mislukt. Aantal pogingen resterend: " + aantalPogingenResterend;
			LOG.error("SE: {} {} {}", seLogCode(screeningsEenheid), accountIdLogTekst(inTeLoggenMedewerker), resultMessage);
		}
		else
		{

			resultMessage = DEFAULT_MESSAGE;
			LOG.error("SE: {} Er is geen medewerker gevonden.", seLogCode(screeningsEenheid));
			logService.logError(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, screeningsEenheid, proxyDatumTijd,
				"Medewerker niet gevonden");
		}
		return resultMessage;
	}

	public OrganisatieMedewerker getIngelogdeOrganisatieMedewerker()
	{
		Long accountId = getIngelogdeAccountId();
		return accountId != null ? hibernateService.load(OrganisatieMedewerker.class, accountId) : null;
	}

	public MammaScreeningsEenheid getIngelogdeScreeningsEenheid()
	{
		return hibernateService.load(MammaScreeningsEenheid.class, screeningsEenheidId);
	}

	public Long getIngelogdeAccountId()
	{
		return accountId;
	}

	public static String accountIdLogTekst(Account account)
	{
		if (account instanceof OrganisatieMedewerker)
		{
			Integer medewerkercode = ((OrganisatieMedewerker) account).getMedewerker().getMedewerkercode();
			return String.format("OM:%s MC:%s", account.getId(), medewerkercode);
		}
		else if (account instanceof Medewerker)
		{
			Integer medewerkercode = ((Medewerker) account).getMedewerkercode();
			return String.format("M:%s MC:%s", account.getId(), medewerkercode);
		}
		return "?";
	}
}
