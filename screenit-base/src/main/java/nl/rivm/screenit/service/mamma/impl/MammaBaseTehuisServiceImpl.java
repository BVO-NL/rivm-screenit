package nl.rivm.screenit.service.mamma.impl;

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

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode_;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.repository.mamma.MammaStandplaatsPeriodeRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseTehuisService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.mamma.MammaStandplaatsSpecification.heeftStandplaatsOpOfNaDatum;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammaBaseTehuisServiceImpl implements MammaBaseTehuisService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private MammaStandplaatsPeriodeRepository standplaatsPeriodeRepository;

	@Autowired
	private SimplePreferenceService preferenceService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Override
	public boolean saveOrUpdateTehuis(MammaTehuis tehuis, InstellingGebruiker ingelogdeGebruiker)
	{
		return saveOrUpdateTehuis(tehuis, null, ingelogdeGebruiker);
	}

	@Override
	public boolean saveOrUpdateTehuis(MammaTehuis tehuis, MammaStandplaats origineleStandplaats, InstellingGebruiker ingelogdeGebruiker)
	{
		String melding = StringUtils.EMPTY;
		String diffToLatestVersion = EntityAuditUtil.getDiffToLatestVersion(tehuis, hibernateService.getHibernateSession());

		boolean isNieuw = tehuis.getId() == null;
		if (isNieuw)
		{
			melding += "Tehuis '" + tehuis.getNaam() + "' aangemaakt.";
		}
		else if (diffToLatestVersion.length() > 0)
		{
			melding += "Tehuis '" + tehuis.getNaam() + "' gewijzigd (" + diffToLatestVersion + ").";
		}

		boolean hasDiffText = StringUtils.isNotEmpty(melding);

		if (hasDiffText)
		{
			logService.logGebeurtenis(LogGebeurtenis.MAMMA_TEHUIS_BEHEER, ingelogdeGebruiker, melding, Bevolkingsonderzoek.MAMMA);
			hibernateService.saveOrUpdate(tehuis.getAanschrijfAdres());
		}
		if (origineleStandplaats != null && !origineleStandplaats.equals(tehuis.getStandplaats()))
		{
			origineleStandplaats.getTehuizen().remove(tehuis);
			hibernateService.saveOrUpdate(origineleStandplaats);
		}

		hibernateService.saveOrUpdateAll(tehuis, tehuis.getStandplaats());
		return hasDiffText;
	}

	@Override
	public MammaStandplaatsRonde getHuidigeStandplaatsRondeVoorStandplaats(MammaStandplaats standplaats)
	{
		var afspraakVanafAantalWerkdagen = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_BIJ_UITNODIGEN_VANAF_AANTAL_WERKDAGEN.name());
		var afsprakenVanafDatum = DateUtil.plusWerkdagen(dateSupplier.getLocalDate(), afspraakVanafAantalWerkdagen);

		var periode = standplaatsPeriodeRepository.findFirst(heeftStandplaatsOpOfNaDatum(standplaats, afsprakenVanafDatum),
			Sort.by(Sort.Direction.ASC, MammaStandplaatsPeriode_.VANAF));
		return periode.map(MammaStandplaatsPeriode::getStandplaatsRonde).orElse(null);
	}
}
