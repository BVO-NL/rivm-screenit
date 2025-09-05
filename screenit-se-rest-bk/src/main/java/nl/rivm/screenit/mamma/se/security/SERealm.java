package nl.rivm.screenit.mamma.se.security;

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

import jakarta.annotation.PostConstruct;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.envers.RevisionInformationResolver;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.security.IScreenitRealm;
import nl.rivm.screenit.security.MultipleAuthenticationSourceCredentialsMatcher;
import nl.rivm.screenit.security.OrganisatieMedewerkerToken;
import nl.rivm.screenit.security.ScreenitPrincipal;
import nl.rivm.screenit.service.BaseMedewerkerService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.ScopeService;
import nl.rivm.screenit.util.MedewerkerUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.yubikey.shiro.YubikeyAuthenticationInfo;
import nl.topicuszorg.yubikey.shiro.YubikeyToken;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
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

@Component
@Slf4j
public class SERealm extends AuthorizingRealm implements IScreenitRealm
{
	private static final int YUBIKEY_SESSION_COUNTER_WRAP_AROUND_ALLOWED_RANGE = 200; 

	private static final int YUBIKEY_MAX_SESSION_COUNTER_INCREASE = 1000; 

	@Autowired
	private BaseMedewerkerService medewerkerService;

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
		setCredentialsMatcher(new MultipleAuthenticationSourceCredentialsMatcher(
			hibernateService, YUBIKEY_SESSION_COUNTER_WRAP_AROUND_ALLOWED_RANGE, YUBIKEY_MAX_SESSION_COUNTER_INCREASE));
		setAuthenticationTokenClass(AuthenticationToken.class);
		RevisionInformationResolver.registerDelegate(new SEAccountResolverDelegate());
	}

	@Override
	protected AuthorizationInfo doGetAuthorizationInfo(PrincipalCollection principals)
	{
		SimpleAuthorizationInfo info = new SimpleAuthorizationInfo();
		ScreenitPrincipal screenitPrincipal = (ScreenitPrincipal) principals.fromRealm(getName()).iterator().next();

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
					if (rol.isRolActief())
					{
						for (Permissie permissie : rol.getRol().getPermissies())
						{
							if (!Boolean.FALSE.equals(permissie.getActief()))
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
			return new SimpleAuthenticationInfo(new ScreenitPrincipal(OrganisatieMedewerker.class, igToken.getId()), null, this.getName());
		}

		return null;
	}

	@Override
	public void clearCachedAuthorizationInfo(OrganisatieMedewerker organisatieMedewerker)
	{
		PrincipalCollection principalCollection = createPrincipalCollection(organisatieMedewerker, false);
		super.clearCachedAuthorizationInfo(principalCollection);
	}

	private PrincipalCollection createPrincipalCollection(OrganisatieMedewerker organisatieMedewerker, boolean checkBvo)
	{
		return new SimplePrincipalCollection(new ScreenitPrincipal(OrganisatieMedewerker.class, organisatieMedewerker.getId(), checkBvo), this.getName());
	}

	@Override
	protected Object getAuthorizationCacheKey(PrincipalCollection principals)
	{
		ScreenitPrincipal principal = (ScreenitPrincipal) principals.getPrimaryPrincipal();
		return principal.getAccountId() + "_" + principal.getAccountClass();
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

		if (permissionResult && permission instanceof Constraint constraint && ((Constraint) permission).isCheckScope())
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
				for (OrganisatieMedewerkerRol rol : instgeb.getRollen())
				{
					if (rol.getActief())
					{
						for (Permissie permissie : rol.getRol().getPermissies())
						{
							Recht recht = permissie.getRecht();
							if (recht.equals(constraint.getRecht()) && !Boolean.FALSE.equals(permissie.getActief())
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
}
