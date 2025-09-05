package nl.rivm.screenit.security;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Arrays;
import java.util.Collection;

import jakarta.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.BaseMedewerkerService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ScopeService;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.yubikey.shiro.YubikeyAuthenticationInfo;
import nl.topicuszorg.yubikey.shiro.YubikeyToken;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.authc.SimpleAuthenticationInfo;
import org.apache.shiro.authc.UsernamePasswordToken;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.authz.Permission;
import org.apache.shiro.authz.SimpleAuthorizationInfo;
import org.apache.shiro.lang.util.SimpleByteSource;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.subject.SimplePrincipalCollection;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import static nl.rivm.screenit.security.UserAgentUtil.getParsedUserAgentInfo;

@Slf4j
@Component
public class ScreenitRealm extends AuthorizingRealm implements IScreenitRealm
{
	@Autowired
	private BaseMedewerkerService medewerkerService;

	@Autowired
	private LogService logService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ScopeService scopeService;

	@Autowired
	@Qualifier(value = "testModus")
	private Boolean testModus;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@PostConstruct
	public void initRealm()
	{
		setCredentialsMatcher(new MultipleAuthenticationSourceCredentialsMatcher(hibernateService));
		setAuthenticationTokenClass(AuthenticationToken.class);
	}

	@Override
	protected AuthorizationInfo doGetAuthorizationInfo(PrincipalCollection principals)
	{
		SimpleAuthorizationInfo info = new SimpleAuthorizationInfo();
		ScreenitPrincipal screenitPrincipal = (ScreenitPrincipal) principals.fromRealm(getName()).iterator().next();
		boolean checkBvo = screenitPrincipal.getCheckBvo();

		if (OrganisatieMedewerker.class.isAssignableFrom(screenitPrincipal.getAccountClass()))
		{
			OrganisatieMedewerker organisatieMedewerker = hibernateService.load(OrganisatieMedewerker.class,
				screenitPrincipal.getAccountId());
			final Medewerker medewerker = organisatieMedewerker.getMedewerker();
			if (Boolean.TRUE.equals(organisatieMedewerker.getActief()) && MedewerkerUtil.isMedewerkerActief(medewerker, currentDateSupplier.getDate()))
			{
				if (LOG.isTraceEnabled())
				{
					LOG.trace("AuthorizationInfo voor " + medewerker.getGebruikersnaam());
				}

				for (OrganisatieMedewerkerRol rol : organisatieMedewerker.getRollen())
				{
					if (rol.isRolActief() && (CollectionUtils.containsAny(rol.getBevolkingsonderzoeken(), organisatieMedewerker.getBevolkingsonderzoeken()) || !checkBvo))
					{
						for (Permissie permissie : rol.getRol().getPermissies())
						{
							if (!Boolean.FALSE.equals(permissie.getActief())
								&& (CollectionUtils.containsAny(rol.getBevolkingsonderzoeken(), Arrays.asList(permissie.getRecht().getBevolkingsonderzoeken()))
								&& CollectionUtils.containsAny(Arrays.asList(permissie.getRecht().getBevolkingsonderzoeken()), organisatieMedewerker.getBevolkingsonderzoeken())
								|| !checkBvo))
							{
								Recht recht = permissie.getRecht();
								if (CollectionUtils.isEmpty(recht.getOrganisatieTypes()) || recht.getOrganisatieTypes()
									.contains(organisatieMedewerker.getOrganisatie().getOrganisatieType()))
								{
									if (Boolean.TRUE.equals(testModus) || !Recht.TESTEN.equals(recht))
									{
										if (LOG.isTraceEnabled())
										{
											LOG.info("* " + recht.name());
										}
										info.addObjectPermission((Permission) HibernateHelper.deproxy(permissie));
									}
								}
							}
						}
					}
				}
			}
		}
		else if (Client.class.isAssignableFrom(screenitPrincipal.getAccountClass()))
		{
			if (LOG.isTraceEnabled())
			{
				LOG.trace("AuthorizationInfo voor client");
			}
			Permissie permissie = new Permissie();
			permissie.setActie(Actie.INZIEN);
			permissie.setRecht(Recht.CLIENT_DASHBOARD);
			permissie.setToegangLevel(ToegangLevel.EIGEN);
			info.addObjectPermission(permissie);
			if (LOG.isTraceEnabled())
			{
				LOG.trace("* " + permissie.getRecht().name());
			}
			Permissie permissie2 = new Permissie();
			permissie2.setActie(Actie.AANPASSEN);
			permissie2.setRecht(Recht.CLIENT_GEGEVENS);
			permissie2.setToegangLevel(ToegangLevel.EIGEN);
			info.addObjectPermission(permissie2);
			if (LOG.isTraceEnabled())
			{
				LOG.trace("* " + permissie2.getRecht().name());
			}
		}

		return info;
	}

	@Override
	protected AuthenticationInfo doGetAuthenticationInfo(AuthenticationToken authcToken)
	{
		if (authcToken instanceof UsernamePasswordToken token)
		{
			Medewerker medewerker = medewerkerService.getMedewerkerByGebruikersnaam(token.getUsername()).orElse(null);
			if (medewerker == null)
			{
				return null;
			}
			if (BooleanUtils.isNotFalse(medewerker.getActief()) && StringUtils.isNotBlank(medewerker.getWachtwoord()))
			{
				if (authcToken instanceof YubikeyToken)
				{
					return new YubikeyAuthenticationInfo(new ScreenitPrincipal(Medewerker.class, medewerker.getId()), medewerker.getWachtwoord(),
						new SimpleByteSource(medewerker.getId().toString()), this.getName(), medewerker.getYubiKey());
				}
				return new SimpleAuthenticationInfo(new ScreenitPrincipal(Medewerker.class, medewerker.getId()), medewerker.getWachtwoord(),
					new SimpleByteSource(medewerker.getId().toString()), this.getName());
			}
		}
		else if (authcToken instanceof OrganisatieMedewerkerToken igToken)
		{
			OrganisatieMedewerker organisatieMedewerker = hibernateService.load(OrganisatieMedewerker.class, igToken.getId());
			String melding = getParsedUserAgentInfo(igToken.getUserAgent()) + ", Organisatie: " + organisatieMedewerker.getOrganisatie().getNaam();
			if (StringUtils.isNotBlank(igToken.getUzipasInlogMethode()))
			{
				melding += ". " + igToken.getUzipasInlogMethode();
			}
			logService.logGebeurtenis(LogGebeurtenis.INLOGGEN, organisatieMedewerker.getMedewerker(), melding);
			return new SimpleAuthenticationInfo(new ScreenitPrincipal(OrganisatieMedewerker.class, igToken.getId()), null, this.getName());
		}
		else if (authcToken instanceof UziToken uziToken)
		{
			Medewerker medewerker = medewerkerService.getMedewerkerByUzinummer((String) uziToken.getPrincipal()).orElse(null);
			if (medewerker == null || !Boolean.TRUE.equals(medewerker.getActief()))
			{
				return null;
			}
			return new SimpleAuthenticationInfo(new ScreenitPrincipal(Medewerker.class, medewerker.getId()), null, this.getName());
		}

		return null;
	}

	@Override
	public void clearCachedAuthorizationInfo(OrganisatieMedewerker organisatieMedewerker)
	{
		PrincipalCollection principalCollection = createPrincipalCollection(organisatieMedewerker, true);
		super.clearCachedAuthorizationInfo(principalCollection);
	}

	private PrincipalCollection createPrincipalCollection(OrganisatieMedewerker organisatieMedewerker, boolean checkBvo)
	{
		return new SimplePrincipalCollection(new ScreenitPrincipal(OrganisatieMedewerker.class, organisatieMedewerker.getId(), checkBvo), this.getName());
	}

	@Override
	public boolean isPermitted(PrincipalCollection principals, Permission permission)
	{
		if (LOG.isTraceEnabled())
		{
			AuthorizationInfo info = getAuthorizationInfo(principals);
			LOG.trace("isPermitted start " + permission.toString());
			if (info.getObjectPermissions() != null)
			{
				for (Permission perm : info.getObjectPermissions())
				{
					if (perm instanceof Permissie permissie)
					{
						LOG.trace("Recht uit authorizationInfo " + permissie.getRecht().name() + " " + perm.implies(permission));
					}
					else
					{
						LOG.trace("Recht uit authorizationInfo " + perm.toString() + " " + perm.implies(permission));
					}
				}
			}
		}

		boolean permissionResult = super.isPermitted(principals, permission);
		if (LOG.isTraceEnabled())
		{
			LOG.trace("permissionResult1 " + permissionResult);
		}

		if (permissionResult && permission instanceof Constraint constraint && constraint.isCheckScope())
		{

			ScreenitPrincipal principal = (ScreenitPrincipal) principals.getPrimaryPrincipal();

			Account account = hibernateService.load(principal.getAccountClass(), principal.getAccountId());
			permissionResult = scopeService.isObjectInScope(constraint, account, principals);

		}
		if (LOG.isTraceEnabled())
		{
			LOG.trace("permissionResult2 " + permissionResult);
		}

		if (permissionResult && permission instanceof Constraint constraint)
		{
			ScreenitPrincipal principal = (ScreenitPrincipal) principals.getPrimaryPrincipal();
			Account account = hibernateService.load(principal.getAccountClass(), principal.getAccountId());
			if (account instanceof OrganisatieMedewerker instgeb)
			{
				permissionResult = false;
				if (!CollectionUtils.isNotEmpty(constraint.getBevolkingsonderzoek()))
				{
					LOG.error("GEEN BVO IN CONSTRAINT!!!!!!!!!"); 
				}
				for (OrganisatieMedewerkerRol rol : instgeb.getRollen())
				{
					if (rol.isRolActief() && CollectionUtils.containsAny(rol.getBevolkingsonderzoeken(), instgeb.getBevolkingsonderzoeken()))
					{
						for (Permissie permissie : rol.getRol().getPermissies())
						{
							Recht recht = permissie.getRecht();
							if (recht.equals(constraint.getRecht()) && !Boolean.FALSE.equals(permissie.getActief())
								&& CollectionUtils.containsAny(rol.getBevolkingsonderzoeken(), constraint.getBevolkingsonderzoek())
								&& CollectionUtils.containsAny(constraint.getBevolkingsonderzoek(), instgeb.getBevolkingsonderzoeken())
								&& (CollectionUtils.isEmpty(recht.getOrganisatieTypes()) || recht.getOrganisatieTypes().contains(instgeb.getOrganisatie().getOrganisatieType())))
							{
								permissionResult = true;
								break;
							}
						}
					}
				}
			}
		}

		if (LOG.isTraceEnabled())
		{
			LOG.trace("permissionResult3 " + permissionResult);
		}

		return permissionResult;
	}

	public Collection<Permissie> getPermissies(PrincipalCollection principals)
	{
		AuthorizationInfo authorizationInfo = getAuthorizationInfo(principals);
		return (Collection) authorizationInfo.getObjectPermissions();
	}

	public Collection<Permissie> getPermissies(OrganisatieMedewerker organisatieMedewerker, boolean checkBvo)
	{
		AuthorizationInfo authorizationInfo = getAuthorizationInfo(createPrincipalCollection(organisatieMedewerker, checkBvo));
		return (Collection) authorizationInfo.getObjectPermissions();
	}
}
