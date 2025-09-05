package nl.rivm.screenit.main.web;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.OvereenkomstService;
import nl.rivm.screenit.main.util.crypto.UZIpasCertInfo;
import nl.rivm.screenit.main.web.gebruiker.algemeen.nieuws.NieuwsPage;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerHoofdMenuItem;
import nl.rivm.screenit.main.web.gebruiker.base.MedewerkerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.login.BvoSelectiePage;
import nl.rivm.screenit.main.web.gebruiker.login.OrganisatieSelectiePage;
import nl.rivm.screenit.main.web.gebruiker.login.OvereenkomstAccoderenPage;
import nl.rivm.screenit.main.web.gebruiker.login.PasswordMustChangePage;
import nl.rivm.screenit.main.web.gebruiker.login.uzipas.zorgid.session.ZorgIdSession;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.Organisatie;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.envers.RevisionInformationResolver.RevisionInformationResolverDelegate;
import nl.rivm.screenit.model.envers.RevisionKenmerk;
import nl.rivm.screenit.model.envers.RevisionKenmerkInThreadHolder;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.security.OrganisatieMedewerkerToken;
import nl.rivm.screenit.security.UziToken;
import nl.rivm.screenit.service.AuthenticatieService;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BaseMedewerkerService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.NieuwsService;
import nl.rivm.screenit.service.ScopeService;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.yubikey.shiro.YubikeyToken;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.IncorrectCredentialsException;
import org.apache.shiro.authc.UnknownAccountException;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.apache.wicket.Component;
import org.apache.wicket.Page;
import org.apache.wicket.Session;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.ISortState;
import org.apache.wicket.injection.Injector;
import org.apache.wicket.markup.html.WebPage;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;
import org.apache.wicket.protocol.http.WebSession;
import org.apache.wicket.request.Request;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.string.Strings;
import org.hibernate.Hibernate;
import org.hibernate.proxy.HibernateProxy;

@Slf4j
public class ScreenitSession extends WebSession
{
	private Long accountId;

	private Class<? extends Account> accountClass;

	private Long currentSelectedMedewerkerId;

	private Long currentSelectedOrganisatieId;

	private Class<? extends Organisatie> currentSelectedOrganisatieClass;

	@SpringBean
	private AuthenticatieService authenticatieService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private OvereenkomstService overeenkomstService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private BaseMedewerkerService medewerkerService;

	@SpringBean
	private ScopeService scopeService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private NieuwsService nieuwsService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private String regioCode;

	private final Set<File> tempFiles = new HashSet<>();

	@Getter
	@Setter
	private boolean isInPlanningmodule;

	private static class ZoekStatus implements IDetachable
	{
		private Long pageNumber;

		private ISortState<?> currentSort;

		private IModel<?> zoekObject;

		public IModel<?> getZoekObject()
		{
			return zoekObject;
		}

		public void setZoekObject(IModel<?> zoekObject)
		{
			this.zoekObject = zoekObject;
		}

		public ISortState<?> getCurrentSort()
		{
			return currentSort;
		}

		public void setCurrentSort(ISortState<?> currentSort)
		{
			this.currentSort = currentSort;
		}

		public Long getPageNumber()
		{
			return pageNumber;
		}

		public void setPageNumber(Long pageNumber)
		{
			this.pageNumber = pageNumber;
		}

		@Override
		public void detach()
		{
			ModelUtil.nullSafeDetach(zoekObject);
		}
	}

	private Map<String, ZoekStatus> zoekStatussen = new HashMap<>();

	private List<String> uzipasMeldingen = new ArrayList<>();

	private boolean fromUitwisselportaal = false;

	private MammobridgeRole mammaHuidigeIDS7Role = MammobridgeRole.RADIOLOGIST;

	private ZorgIdSession zorgIdSession;

	private boolean uziPasTokenAfgekeurd = false;

	private boolean ingelogdMetZorgId = false;

	public ScreenitSession(Request request)
	{
		super(request);
		Injector.get().inject(this);
	}

	public static ScreenitSession get()
	{
		return (ScreenitSession) Session.get();
	}

	public String getRegioCode()
	{
		return regioCode;
	}

	public void setRegioCode(String regioCode)
	{
		this.regioCode = regioCode;
	}

	public boolean isSignedIn()
	{
		Subject currentUser = SecurityUtils.getSubject();
		return currentUser.isAuthenticated();
	}

	@Override
	public void replaceSession()
	{

		Subject currentUser = SecurityUtils.getSubject();
		currentUser.getSession().stop();
		super.replaceSession();
		currentUser.getSession(true);
	}

	public void logout()
	{
		Subject currentUser = SecurityUtils.getSubject();
		currentUser.logout();
		this.invalidate();
	}

	public Page login(String gebruikersnaam, String plainWachtwoord, String yubikeyOTP)
	{
		SecurityManager securityManager = SecurityUtils.getSecurityManager();
		uziPasTokenAfgekeurd = false;
		YubikeyToken token = new YubikeyToken(gebruikersnaam, plainWachtwoord, yubikeyOTP);
		Page result = null;
		Medewerker medewerker = null;
		try
		{
			medewerker = medewerkerService.getMedewerkerByGebruikersnaam(token.getUsername()).orElse(null);
			if (medewerker != null && medewerker.getInlogMethode() != InlogMethode.YUBIKEY)
			{
				error(getString(medewerker.getInlogMethode().getLoginErrorMsg(), null));
				return null;
			}

			securityManager.authenticate(token);

			String meldingNavActiefVanafEnTotEnMet = MedewerkerUtil.meldingNavActiefVanafEnTotEnMet(medewerker, currentDateSupplier.getDate());
			if (meldingNavActiefVanafEnTotEnMet != null)
			{
				error(meldingNavActiefVanafEnTotEnMet);
			}
			else
			{
				result = login(gebruikersnaam, medewerker);
			}
		}
		catch (UnknownAccountException | IncorrectCredentialsException ice)
		{
			if (getFeedbackMessages().isEmpty())
			{
				authenticatieService.foutieveInlogpoging(medewerker);
				getError(medewerker, gebruikersnaam);
				if (medewerker == null || Boolean.TRUE.equals(medewerker.getActief()))
				{
					error(getString("error.authentication", null));
				}
			}
		}
		catch (AuthenticationException authExcept)
		{
			error(getString("error.authentication", null));
		}

		return result;
	}

	public Page login(String gebruikersnaam, String plainWachtwoord)
	{
		SecurityManager securityManager = SecurityUtils.getSecurityManager();
		uziPasTokenAfgekeurd = false;
		UsernamePasswordToken token = new UsernamePasswordToken(gebruikersnaam, plainWachtwoord);
		Page result = null;
		Medewerker medewerker = null;
		try
		{
			medewerker = medewerkerService.getMedewerkerByGebruikersnaam(token.getUsername()).orElse(null);

			if (medewerker != null && medewerker.getInlogMethode() != InlogMethode.GEBRUIKERSNAAM_WACHTWOORD)
			{
				error(getString(medewerker.getInlogMethode().getLoginErrorMsg(), null));
				return null;
			}

			securityManager.authenticate(token);

			String meldingNavActiefVanafEnTotEnMet = MedewerkerUtil.meldingNavActiefVanafEnTotEnMet(medewerker, currentDateSupplier.getDate());
			if (meldingNavActiefVanafEnTotEnMet != null)
			{
				error(meldingNavActiefVanafEnTotEnMet);
			}
			else
			{
				result = login(gebruikersnaam, medewerker);
			}
		}
		catch (UnknownAccountException | IncorrectCredentialsException ice)
		{
			if (getFeedbackMessages().isEmpty())
			{
				authenticatieService.foutieveInlogpoging(medewerker);
				getError(medewerker, gebruikersnaam);
				if (medewerker == null || Boolean.TRUE.equals(medewerker.getActief()))
				{
					error(getString("error.authentication", null));
				}
			}
		}
		catch (AuthenticationException authExcept)
		{
			error(getString("error.authentication", null));
		}

		return result;
	}

	private Page login(String gebruikersnaam, Medewerker medewerker)
	{
		Page result = null;
		zoekStatussen = new HashMap<>();

		setIngelogdeAccount(medewerker);

		if (medewerker != null && !authenticatieService.isAccountLocked(medewerker))
		{
			authenticatieService.unlockAccount(medewerker);

			List<OrganisatieMedewerker> organisatieMedewerkers = authenticatieService.getActieveOrganisatieMedewerkers(medewerker);
			if (organisatieMedewerkers.isEmpty())
			{

				logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, medewerker, "Gebruikersnaam: " + medewerker.getGebruikersnaam()
					+ ", geen actieve organisatie gekoppeld aan medewerker of niet voldoende rechten voor gekoppelde organisatie(s).");

				error(getString("error.nietvoldoende.rechten", null));
			}
			else if (organisatieMedewerkers.size() == 1)
			{

				OrganisatieMedewerker inTeLoggenOrganisatieMedewerker = organisatieMedewerkers.get(0);
				result = getPageForOrganisatieMedewerker(inTeLoggenOrganisatieMedewerker);
			}
			else
			{

				result = new OrganisatieSelectiePage(medewerker);
			}
		}
		else if (gebruikersnaam != null)
		{
			getError(medewerker, gebruikersnaam);
		}
		else
		{
			if (authenticatieService.isAccountLocked(medewerker))
			{
				error("Uw account is geblokkeerd. Neem contact op met uw beheerder.");
			}
		}
		return result;
	}

	public void login(OrganisatieMedewerker organisatieMedewerker)
	{
		Subject currentUser = SecurityUtils.getSubject();
		OrganisatieMedewerkerToken token = new OrganisatieMedewerkerToken(organisatieMedewerker.getId());
		token.setUserAgent(WebSession.get().getClientInfo().getUserAgent());
		if (ingelogdMetZorgId)
		{
			token.setUzipasInlogMethode("UZI-pas met Zorg-ID");
		}
		try
		{
			currentUser.login(token);
			setIngelogdeAccount(organisatieMedewerker);
			resetZoekStatus();
		}
		catch (UnknownAccountException | IncorrectCredentialsException ice)
		{
			if (getFeedbackMessages().isEmpty())
			{
				error(getString("error.authentication", null));
			}
		}
	}

	public Page getPageForOrganisatieMedewerker(OrganisatieMedewerker inTeLoggenOrganisatieMedewerker)
	{
		WebPage result = null;

		if (autorisatieService.mustChangePassword(inTeLoggenOrganisatieMedewerker))
		{
			result = new PasswordMustChangePage(inTeLoggenOrganisatieMedewerker);
		}
		else if (overeenkomstService.countTeAccoderenOvereenkomsten(inTeLoggenOrganisatieMedewerker) > 0)
		{
			result = new OvereenkomstAccoderenPage(ModelUtil.sModel(inTeLoggenOrganisatieMedewerker));
		}
		else if (CollectionUtils.isEmpty(inTeLoggenOrganisatieMedewerker.getBevolkingsonderzoeken()))
		{
			result = new BvoSelectiePage(inTeLoggenOrganisatieMedewerker);
		}
		else
		{
			login(inTeLoggenOrganisatieMedewerker);
			try
			{
				if (!nieuwsService.getNieuwsItemIdsMedewerker(inTeLoggenOrganisatieMedewerker.getMedewerker()).isEmpty())
				{
					result = new NieuwsPage();
				}
				else
				{
					result = getHomePage().newInstance();
				}
				if (checkPermission(Recht.MEDEWERKER_SCREENING_MAMMA_BEOORDELING_WERKLIJST, Actie.INZIEN) && result instanceof MedewerkerBasePage)
				{
					((MedewerkerBasePage) result).setLoginIms(true);
				}
			}
			catch (InstantiationException | IllegalAccessException | NullPointerException e)
			{
				String msg = "";
				if (e.getMessage() != null)
				{
					msg += e.getMessage();
				}
				msg += "\n" + e.getClass().getName();
				if (e.getStackTrace() != null)
				{
					int logDiepte = e.getStackTrace().length;
					if (logDiepte > 6)
					{
						logDiepte = 6;
					}
					for (int i = 0; i < logDiepte; i++)
					{
						msg += "\n\t at " + e.getStackTrace()[i];
					}
				}
				LOG.error("Fout bij bepalen homepage: " + msg, e);
			}
		}
		return result;
	}

	private Class<? extends WebPage> getHomePage()
	{
		for (MedewerkerHoofdMenuItem hoofdMenuItem : MedewerkerHoofdMenuItem.values())
		{
			Class<? extends MedewerkerBasePage> targetPage = MedewerkerMenuItem.getTargetPageClass(hoofdMenuItem.getMenuItem());
			if (targetPage != null && getAuthorizationStrategy().isInstantiationAuthorized(targetPage))
			{
				return targetPage;

			}
		}

		return null;
	}

	private void getError(Medewerker inTeLoggenMedewerker, String gebruikersnaam)
	{
		Integer foutieveAanmeldpogingenTimeout = preferenceService.getInteger(PreferenceKey.FOUTIEVE_AANMELDPOGINGEN_TIMEOUT.name());
		if (foutieveAanmeldpogingenTimeout == null)
		{
			foutieveAanmeldpogingenTimeout = 30;
		}

		if (inTeLoggenMedewerker != null && Boolean.FALSE.equals(inTeLoggenMedewerker.getActief()))
		{
			error("Uw account is gedeactiveerd. Neem contact op met uw beheerder.");
			logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, "Inlogpoging met een gedeactiveerd account");
		}
		else if (authenticatieService.isAccountLocked(inTeLoggenMedewerker))
		{
			if (InlogStatus.GEBLOKKEERD.equals(inTeLoggenMedewerker.getInlogstatus()))
			{
				error("Uw account is geblokkeerd. Neem contact op met uw beheerder.");
			}
			else if (InlogStatus.TIJDELIJK_GEBLOKKEERD.equals(inTeLoggenMedewerker.getInlogstatus()))
			{
				error("Uw account is voor " + foutieveAanmeldpogingenTimeout + " minuten geblokkeerd");
			}
			logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, "Inlogpoging met een geblokkeerd account");
		}
		else if (inTeLoggenMedewerker != null && InlogStatus.OK.equals(inTeLoggenMedewerker.getInlogstatus()) && inTeLoggenMedewerker.getFoutieveInlogpogingen() != null)
		{
			switch (inTeLoggenMedewerker.getInlogMethode())
			{
			case GEBRUIKERSNAAM_WACHTWOORD:
				logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, "Inlogpoging met onjuist wachtwoord");
				break;
			case YUBIKEY:
				logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, "Inlogpoging met onjuist wachtwoord of YubiKey OTP");
				break;
			}
		}
		else
		{
			var melding = gebruikersnaam == null ?
				"Inloggen mislukt omdat er geen gebruikersnaam en wachtwoord ingevuld zijn" :
				"Medewerker niet gevonden: " + Strings.escapeMarkup(gebruikersnaam, false, false).toString();
			logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, inTeLoggenMedewerker, melding);
		}
	}

	private String getString(String resourceKey, Component comp)
	{
		return getApplication().getResourceSettings().getLocalizer().getString(resourceKey, comp);
	}

	public Organisatie getOrganisatie()
	{
		Account ingelogdAccount = getIngelogdAccount();
		Organisatie organisatie = null;
		if (ingelogdAccount instanceof OrganisatieMedewerker organisatieMedewerker)
		{
			organisatie = organisatieMedewerker.getOrganisatie();
		}
		return organisatie;
	}

	public ScreeningOrganisatie getScreeningOrganisatie()
	{

		Organisatie organisatie = hibernateService.deproxy(getOrganisatie());
		if (OrganisatieType.SCREENINGSORGANISATIE.equals(organisatie.getOrganisatieType()))
		{
			return (ScreeningOrganisatie) organisatie;
		}
		return null;
	}

	public ColonIntakelocatie getIntakelocatie()
	{
		Organisatie organisatie = getOrganisatie();
		if (ColonIntakelocatie.class.isAssignableFrom(Hibernate.getClass(organisatie)))
		{
			if (!(organisatie instanceof ColonIntakelocatie))
			{
				HibernateProxy hibernateProxy = (HibernateProxy) organisatie;
				organisatie = (ColonIntakelocatie) hibernateProxy.getHibernateLazyInitializer().getImplementation();
			}
			return (ColonIntakelocatie) organisatie;
		}
		return null;
	}

	public OrganisatieMedewerker getIngelogdeOrganisatieMedewerker()
	{
		Account account = getIngelogdAccount();
		if (account instanceof OrganisatieMedewerker)
		{
			return (OrganisatieMedewerker) account;
		}
		return null;
	}

	public Account getIngelogdAccount()
	{
		return accountId != null ? hibernateService.load(accountClass, accountId) : null;
	}

	public void setIngelogdeAccount(Account account)
	{
		accountId = account != null ? account.getId() : null;
		accountClass = account != null ? HibernateHelper.getDeproxiedClass(account) : null;

		OrganisatieMedewerker organisatieMedewerker = getIngelogdeOrganisatieMedewerker();
		if (organisatieMedewerker != null)
		{
			LOG.info("OrganisatieMedewerker (id: '{}') is aan het inloggen namens {}. (Gid: '{}')", organisatieMedewerker.getId(),
				organisatieMedewerker.getOrganisatie().getNaam(),
				organisatieMedewerker.getMedewerker().getId());
		}
	}

	public Serializable getLoggedInAccountId()
	{
		return accountId;
	}

	public Class<? extends Account> getLoggedInAccountClass()
	{
		return accountClass;
	}

	public boolean checkPermission(Recht recht, Actie actie)
	{
		return checkPermission(recht, actie, null);
	}

	public boolean checkPermission(Recht recht, Actie actie, HibernateObject scopeObject)
	{
		Constraint constraintToCheck = new Constraint();
		constraintToCheck.setRecht(recht);
		constraintToCheck.setActie(actie);
		constraintToCheck.setBevolkingsonderzoek(getOnderzoeken());
		constraintToCheck.setCheckScope(scopeObject != null);
		if (scopeObject != null)
		{
			constraintToCheck.setScopeObjectClass(ModelProxyHelper.getClass(scopeObject));
			constraintToCheck.setScopeObjectId(scopeObject.getId());
		}

		return ThreadContext.getSecurityManager().isPermitted(getPrincipalCollection(), constraintToCheck);
	}

	private PrincipalCollection getPrincipalCollection()
	{
		return SecurityUtils.getSubject().getPrincipals();
	}

	@Override
	public void detach()
	{
		for (ZoekStatus status : zoekStatussen.values())
		{
			ModelUtil.nullSafeDetach(status);
		}

		super.detach();
	}

	public Medewerker getCurrentSelectedMedewerker()
	{
		return currentSelectedMedewerkerId != null ? hibernateService.load(Medewerker.class, currentSelectedMedewerkerId) : null;
	}

	public void setCurrentSelectedMedewerker(Medewerker medewerker)
	{
		currentSelectedMedewerkerId = medewerker != null ? medewerker.getId() : null;
	}

	public Organisatie getCurrentSelectedOrganisatie()
	{
		return currentSelectedOrganisatieId != null ? hibernateService.load(currentSelectedOrganisatieClass, currentSelectedOrganisatieId) : null;
	}

	public void setCurrentSelectedOrganisatie(Organisatie organisatie)
	{
		currentSelectedOrganisatieId = organisatie != null ? organisatie.getId() : null;
		currentSelectedOrganisatieClass = organisatie != null ? HibernateHelper.getDeproxiedClass(organisatie) : null;
	}

	public boolean loginUzipasZonderApplet(UZIpasCertInfo uziCertInfo)
	{
		this.ingelogdMetZorgId = true;
		uzipasMeldingen = new ArrayList<>();
		UziToken uziToken = new UziToken(uziCertInfo.getUziCode());
		SecurityManager securityManager = SecurityUtils.getSecurityManager();

		try
		{
			securityManager.authenticate(uziToken);
			var medewerker = medewerkerService.getMedewerkerByUzinummer(uziCertInfo.getUziCode()).orElse(null);

			if (medewerker != null && medewerker.getInlogMethode() != InlogMethode.UZIPAS)
			{
				uzipasMeldingen.add(getString(medewerker.getInlogMethode().getLoginErrorMsg(), null));
				return false;
			}

			setIngelogdeAccount(medewerker);

			if (medewerker != null && !authenticatieService.isAccountLocked(medewerker))
			{
				authenticatieService.unlockAccount(medewerker);

				List<OrganisatieMedewerker> organisatieMedewerkers = authenticatieService.getActieveOrganisatieMedewerkers(medewerker);
				if (organisatieMedewerkers.isEmpty())
				{

					logService.logGebeurtenis(LogGebeurtenis.INLOGGEN_MISLUKT, medewerker, "Gebruikersnaam: " + medewerker.getGebruikersnaam()
						+ ", geen actieve organisatie gekoppeld aan medewerker of niet voldoende rechten voor gekoppelde organisatie(s).");

					uzipasMeldingen.add(getString("error.nietvoldoende.rechten", null));
				}
				else if (organisatieMedewerkers.size() == 1)
				{

					OrganisatieMedewerker inTeLoggenOrganisatieMedewerker = organisatieMedewerkers.get(0);
					if (getPageForOrganisatieMedewerker(inTeLoggenOrganisatieMedewerker) == null)
					{
						uzipasMeldingen.add("Niet genoeg rechten om in te loggen");
					}
				}
			}
			else
			{
				if (authenticatieService.isAccountLocked(medewerker))
				{
					uzipasMeldingen.add("Uw account is geblokkeerd. Neem contact op met uw beheerder.");
				}
			}
			if (uzipasMeldingen.isEmpty())
			{
				String meldingNavActiefVanafEnTotEnMet = MedewerkerUtil.meldingNavActiefVanafEnTotEnMet(medewerker, currentDateSupplier.getDate());
				if (meldingNavActiefVanafEnTotEnMet != null)
				{
					uzipasMeldingen.add(meldingNavActiefVanafEnTotEnMet);
				}
			}
		}
		catch (UnknownAccountException | IncorrectCredentialsException ice)
		{
			LOG.error("Fout bij inloggen met smartcard ", ice);
			if (getFeedbackMessages().isEmpty())
			{
				uzipasMeldingen.add(String.format(getString("error.uziauthentication", null), uziCertInfo.getUziCode()));
			}
		}
		return uzipasMeldingen.isEmpty();
	}

	public Long getSavedPageNumber(String compPatch)
	{
		return getZoekStatus(compPatch).getPageNumber();
	}

	public void setSavedPageNumber(String compPatch, Long pageNumber)
	{
		getZoekStatus(compPatch).setPageNumber(pageNumber);
	}

	public ISortState getCurrentSort(String compPatch)
	{
		return getZoekStatus(compPatch).getCurrentSort();
	}

	public void setCurrentSort(String compPatch, ISortState iSortState)
	{
		getZoekStatus(compPatch).setCurrentSort(iSortState);
	}

	public IModel<?> getZoekObject(Class<? extends Component> compClass)
	{
		return getZoekStatus(compClass).getZoekObject();
	}

	public IModel<?> getZoekObject(String key)
	{
		return getZoekStatus(key).getZoekObject();
	}

	public boolean isZoekObjectGezetForComponent(String key)
	{
		return getZoekObject(key) != null;
	}

	public boolean isZoekObjectGezetForComponent(Class<? extends Component> cls)
	{
		return getZoekObject(cls) != null;
	}

	public void setZoekObject(Class<? extends Component> compClass, IModel<?> zoekObject)
	{
		setZoekObject(compClass.toString(), zoekObject);
	}

	public void setZoekObject(String key, IModel<?> zoekObject)
	{
		ZoekStatus status = getZoekStatus(key);
		status.setZoekObject(zoekObject);
	}

	private ZoekStatus getZoekStatus(Class<? extends Component> compClass)
	{
		return getZoekStatus(compClass.toString());
	}

	private ZoekStatus getZoekStatus(String key)
	{
		ZoekStatus status = zoekStatussen.get(key);
		if (status == null)
		{
			status = new ZoekStatus();
			zoekStatussen.put(key, status);
		}
		return status;
	}

	public void resetZoekStatus()
	{
		zoekStatussen = new HashMap<>();
	}

	public List<Bevolkingsonderzoek> getOnderzoeken()
	{
		OrganisatieMedewerker organisatieMedewerker = getIngelogdeOrganisatieMedewerker();
		if (organisatieMedewerker != null && CollectionUtils.isNotEmpty(organisatieMedewerker.getBevolkingsonderzoeken()))
		{
			return Bevolkingsonderzoek.sort(new ArrayList<>(organisatieMedewerker.getBevolkingsonderzoeken()));
		}
		else
		{
			return new ArrayList<>();
		}
	}

	public ToegangLevel getToegangsLevel(Actie actie, Recht recht)
	{
		Constraint constraintToCheck = new Constraint();
		constraintToCheck.setActie(actie);
		constraintToCheck.setRecht(recht);
		constraintToCheck.setBevolkingsonderzoek(getOnderzoeken());
		return scopeService.getHoogsteToegangLevel(getPrincipalCollection(), constraintToCheck);
	}

	public ToegangLevel getToegangsLevel(OrganisatieMedewerker organisatieMedewerker, Actie actie, Recht recht, boolean checkBvo)
	{
		Constraint constraintToCheck = new Constraint();
		constraintToCheck.setActie(actie);
		constraintToCheck.setRecht(recht);
		return scopeService.getHoogsteToegangLevel(organisatieMedewerker, constraintToCheck, checkBvo);
	}

	public static class ScreenitSessionRevisionInformationResolverDelegate implements RevisionInformationResolverDelegate
	{
		@Override
		public Account getAccount()
		{
			if (Session.exists())
			{
				return ScreenitSession.get().getIngelogdAccount();
			}
			return null;
		}

		@Override
		public RevisionKenmerk getRevisionKenmerk()
		{
			return RevisionKenmerkInThreadHolder.getKenmerk();
		}
	}

	public Page getUzipasPage(boolean fromUitwisselportaal)
	{
		this.fromUitwisselportaal = fromUitwisselportaal;
		Account ingelogdAccount = getIngelogdAccount();
		if (ingelogdAccount instanceof OrganisatieMedewerker)
		{
			return login(null, ((OrganisatieMedewerker) ingelogdAccount).getMedewerker());
		}
		return login(null, (Medewerker) ingelogdAccount);
	}

	public List<String> getUzipasMeldingen()
	{
		return uzipasMeldingen;
	}

	public void addTempFile(File tempFile)
	{
		tempFiles.add(tempFile);
	}

	public UploadDocument fileUploadToUploadDocument(FileUpload fileUpload) throws Exception
	{
		UploadDocument uploadDocument = null;
		if (fileUpload != null)
		{
			uploadDocument = new UploadDocument();
			File tempFile = fileUpload.writeToTempFile();
			tempFile.deleteOnExit();
			uploadDocument.setFile(tempFile);
			uploadDocument.setActief(true);
			uploadDocument.setNaam(fileUpload.getClientFileName());
			uploadDocument.setContentType(fileUpload.getContentType());
			addTempFile(tempFile);
		}
		return uploadDocument;
	}

	@Override
	public void invalidate()
	{
		getZorgIdSession().clear();
		uziPasTokenAfgekeurd = false;
		for (File tempFile : tempFiles)
		{
			FileUtils.deleteQuietly(tempFile);
		}
		super.invalidate();
	}

	public boolean isFromUitwisselportaal()
	{
		return fromUitwisselportaal;
	}

	public MammobridgeRole wijzigMammaIDS7Role(MammobridgeRole role)
	{
		MammobridgeRole oudeRole = getMammaHuidigeIDS7Role();
		this.mammaHuidigeIDS7Role = role;

		return oudeRole;
	}

	public MammobridgeRole getMammaHuidigeIDS7Role()
	{
		return mammaHuidigeIDS7Role;
	}

	public ZorgIdSession getZorgIdSession()
	{
		if (zorgIdSession == null)
		{
			zorgIdSession = new ZorgIdSession();
		}
		return zorgIdSession;
	}

	public boolean isUziPasTokenAfgekeurd()
	{
		return uziPasTokenAfgekeurd;
	}

	public void setUziPasTokenAfgekeurd(boolean uziPasTokenAfgekeurd)
	{
		this.uziPasTokenAfgekeurd = uziPasTokenAfgekeurd;
	}

	public boolean inScope(SecurityConstraint constraint)
	{
		if (constraint.checkScope() && constraint.organisatieTypeScopes().length > 0)
		{
			Organisatie organisatie = getOrganisatie();
			boolean valtBinnenOrganisatieTypeScopes = false;
			if (organisatie != null)
			{
				for (OrganisatieType type : constraint.organisatieTypeScopes())
				{
					if (organisatie.getOrganisatieType().equals(type))
					{
						valtBinnenOrganisatieTypeScopes = true;
						break;
					}
				}
			}
			return valtBinnenOrganisatieTypeScopes;
		}
		return true;
	}

}
