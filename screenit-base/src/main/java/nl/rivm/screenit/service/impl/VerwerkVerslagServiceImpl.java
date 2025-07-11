package nl.rivm.screenit.service.impl;

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
import java.util.Date;

import jakarta.annotation.PostConstruct;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.VerslagProjectVersionMapping;
import nl.rivm.screenit.model.berichten.enums.VerslagGeneratie;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.colon.PaVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.service.VerwerkVerslagService;
import nl.rivm.screenit.service.cervix.CervixVerwerkVerslagService;
import nl.rivm.screenit.service.colon.ColonVerwerkVerslagService;
import nl.rivm.screenit.service.mamma.MammaVerwerkVerslagService;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class VerwerkVerslagServiceImpl implements VerwerkVerslagService
{
	@Autowired
	@Qualifier("versionMapping")
	private String versionMapping;

	@Autowired(required = false)
	private ColonVerwerkVerslagService colonVerwerkVerslagService;

	@Autowired(required = false)
	private CervixVerwerkVerslagService cervixVerwerkVerslagService;

	@Autowired(required = false)
	private MammaVerwerkVerslagService mammaVerwerkVerslagService;

	@PostConstruct
	public void init()
	{
		if (StringUtils.isNotBlank(versionMapping))
		{
			var splittedVersionMapping = versionMapping.trim().split(";");
			for (var versionMap : splittedVersionMapping)
			{
				var splittedVersionMap = versionMap.split("\\|");

				var generatie = VerslagGeneratie.valueOf(splittedVersionMap[0]);
				var projectVersions = splittedVersionMap[1].split(",");
				Arrays.stream(projectVersions).forEach(projectVersion -> VerslagProjectVersionMapping.get()
					.addProjectVersion(projectVersion, generatie, VerslagType.MDL, VerslagType.PA_LAB, VerslagType.CERVIX_CYTOLOGIE));
			}
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void verwerkInDossier(Verslag verslag)
	{
		switch (verslag.getType())
		{
		case MDL:
			colonVerwerkVerslagService.verwerkInDossier((MdlVerslag) verslag);
			break;
		case PA_LAB:
			break;
		case CERVIX_CYTOLOGIE:
			cervixVerwerkVerslagService.verwerkInDossier((CervixCytologieVerslag) verslag);
			break;
		case MAMMA_PA_FOLLOW_UP:
		case MAMMA_PA_FOLLOW_UP_MONITOR:
			mammaVerwerkVerslagService.verwerkVerslagInDossier((MammaFollowUpVerslag) verslag);
			break;
		}
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void onAfterVerwerkVerslagContent(Verslag verslag)
	{
		switch (verslag.getType())
		{
		case MDL:
			colonVerwerkVerslagService.onAfterVerwerkVerslagContent((MdlVerslag) verslag);
			break;
		case PA_LAB:
			colonVerwerkVerslagService.onAfterVerwerkVerslagContent((PaVerslag) verslag);
			break;
		case CERVIX_CYTOLOGIE:
			cervixVerwerkVerslagService.onAfterVerwerkVerslagContent((CervixCytologieVerslag) verslag);
			break;
		case MAMMA_PA_FOLLOW_UP:
		case MAMMA_PA_FOLLOW_UP_MONITOR:
			mammaVerwerkVerslagService.onAfterVerwerkVerslagContent((MammaFollowUpVerslag) verslag);
			break;
		}
	}

	@Override
	public ScreeningRonde getValideScreeningsRonde(VerslagType type, Client client, Verslag olderVerslag, Date onderzoeksdatum)
	{
		switch (type)
		{
		case MDL:
		case PA_LAB:
			return colonVerwerkVerslagService.getValideScreeningsRonde(client, olderVerslag, onderzoeksdatum);
		case CERVIX_CYTOLOGIE:
			break;
		case MAMMA_PA_FOLLOW_UP:
		case MAMMA_PA_FOLLOW_UP_MONITOR:
			return mammaVerwerkVerslagService.getValideScreeningsRonde(client, onderzoeksdatum);
		}
		return null;
	}

}
