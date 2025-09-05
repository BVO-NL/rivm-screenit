package nl.rivm.screenit.mamma.se.service.impl;

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

import java.time.LocalDate;

import nl.rivm.screenit.mamma.se.dto.SERechtDto;
import nl.rivm.screenit.mamma.se.dto.SeAutorisatieDto;
import nl.rivm.screenit.mamma.se.security.SERealm;
import nl.rivm.screenit.mamma.se.service.SeAutorisatieService;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.OrganisatieMedewerkerRol;
import nl.rivm.screenit.model.Permissie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.security.Constraint;
import nl.rivm.screenit.security.ScreenitPrincipal;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.ArrayUtils;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.subject.PrincipalCollection;
import org.apache.shiro.subject.SimplePrincipalCollection;
import org.apache.shiro.util.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SeAutorisatieServiceImpl implements SeAutorisatieService
{

	@Autowired
	private AutorisatieService autorisatieService;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private SERealm seRealm;

	@Override
	public boolean isGeautoriseerd(Long accountId, Recht recht)
	{
		return checkPermission(accountId, recht, getBenodigdeActie(recht));
	}

	@Override
	public boolean isGeautoriseerdVoorInloggen(Long organisatieMedewerkerId)
	{
		return isGeautoriseerd(organisatieMedewerkerId, Recht.MEDEWERKER_SCREENING_MAMMA_SE_INSCHRIJVEN)
			|| isGeautoriseerd(organisatieMedewerkerId, Recht.MEDEWERKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN);
	}

	@Override
	public boolean isOrganisatieMedewerkerGeautoriseerd(OrganisatieMedewerker organisatieMedewerker, Recht recht)
	{

		return autorisatieService.getActieVoorMedewerker(organisatieMedewerker, null, recht) == getBenodigdeActie(recht);
	}

	@Override
	public SeAutorisatieDto getSeRechten(Long accountId)
	{
		SeAutorisatieDto dto = new SeAutorisatieDto();
		OrganisatieMedewerker organisatieMedewerker = hibernateService.get(OrganisatieMedewerker.class, accountId);
		PrincipalCollection principals = createSimplePrincipalCollection(accountId);
		SecurityManager securityManager = ThreadContext.getSecurityManager();

		dto.setInschrijvenRecht(getSeRechtDto(organisatieMedewerker, securityManager, principals, Recht.MEDEWERKER_SCREENING_MAMMA_SE_INSCHRIJVEN));
		dto.setOnderzoekenRecht(getSeRechtDto(organisatieMedewerker, securityManager, principals, Recht.MEDEWERKER_SCREENING_MAMMA_SE_ONDERZOEK));
		dto.setSignalerenRecht(getSeRechtDto(organisatieMedewerker, securityManager, principals, Recht.MEDEWERKER_SCREENING_MAMMA_SE_SIGNALEREN));
		dto.setKwaliteitsopnameRecht(getSeRechtDto(organisatieMedewerker, securityManager, principals, Recht.MEDEWERKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME));
		dto.setConnectiestatusRecht(getSeRechtDto(organisatieMedewerker, securityManager, principals, Recht.MEDEWERKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN));

		return dto;
	}

	private SERechtDto getSeRechtDto(OrganisatieMedewerker organisatieMedewerker, SecurityManager securityManager, PrincipalCollection principalCollection, Recht recht)
	{
		SERechtDto seRechtDto = new SERechtDto();
		Actie benodigdeActie = getBenodigdeActie(recht);
		seRechtDto.setAuthorized(securityManager.isPermitted(principalCollection, buildConstraint(recht, benodigdeActie)));
		setEinddatumOpBasisVanRollen(seRechtDto, organisatieMedewerker, recht, benodigdeActie);

		LocalDate gebruikerActiefTotEnMet = DateUtil.toLocalDate(organisatieMedewerker.getMedewerker().getActiefTotEnMet());
		if (gebruikerActiefTotEnMet != null && (seRechtDto.getEindDatum() == null || gebruikerActiefTotEnMet.compareTo(seRechtDto.getEindDatum()) < 0))
		{
			seRechtDto.setEindDatum(gebruikerActiefTotEnMet);
		}
		return seRechtDto;
	}

	private void setEinddatumOpBasisVanRollen(SERechtDto seRechtDto, OrganisatieMedewerker organisatieMedewerker, Recht recht, Actie benodigdeActie)
	{
		LocalDate maxEindDatum = null;

		for (OrganisatieMedewerkerRol organisatieMedewerkerRol : organisatieMedewerker.getRollen())
		{
			if (organisatieMedewerkerRol.isRolActief())
			{
				for (Permissie permissie : organisatieMedewerkerRol.getRol().getPermissies())
				{
					if (permissie.getRecht().equals(recht) && ArrayUtils.contains(permissie.getRecht().getActie(), benodigdeActie))
					{
						LocalDate eindDatum = DateUtil.toLocalDate(organisatieMedewerkerRol.getEindDatum());
						if (eindDatum == null)
						{
							seRechtDto.setEindDatum(null);
							return;
						}
						else
						{
							if (maxEindDatum == null || eindDatum.compareTo(maxEindDatum) > 0)
							{
								maxEindDatum = eindDatum;
							}
						}
					}
				}
			}
		}
		seRechtDto.setEindDatum(maxEindDatum);
	}

	private Actie getBenodigdeActie(Recht recht)
	{
		switch (recht)
		{
		case MEDEWERKER_SCREENING_MAMMA_SE_KWALITEITSOPNAME:
			return Actie.TOEVOEGEN;
		case MEDEWERKER_SCREENING_MAMMA_SE_CONNECTIESTATUS_INZIEN:
			return Actie.INZIEN;
		default:
			return Actie.VERWIJDEREN;
		}
	}

	private boolean checkPermission(Long accountId, Recht recht, Actie actie)
	{
		return ThreadContext.getSecurityManager().isPermitted(createSimplePrincipalCollection(accountId), buildConstraint(recht, actie));
	}

	private Constraint buildConstraint(Recht recht, Actie actie)
	{
		Constraint constraintToCheck = new Constraint();
		constraintToCheck.setRecht(recht);
		constraintToCheck.setActie(actie);
		constraintToCheck.setCheckScope(false);
		return constraintToCheck;
	}

	private SimplePrincipalCollection createSimplePrincipalCollection(Long accountId)
	{
		return new SimplePrincipalCollection(new ScreenitPrincipal(OrganisatieMedewerker.class, accountId, false), seRealm.getName());
	}
}
