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

import java.util.EnumMap;
import java.util.Map;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.se.dto.SeAutorisatieDto;
import nl.rivm.screenit.mamma.se.dto.SeConfiguratieKey;
import nl.rivm.screenit.mamma.se.service.ConfiguratieService;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ConfiguratieServiceImpl implements ConfiguratieService
{

	private static final int SE_PING_INTERVAL_DEFAULT = 2500;

	private static final int SE_PONG_TIMEOUT_DEFAULT = 5000;

	private static final String INTERNAL_MAMMA_SE_INFORMATIE_OPHALEN_CRON_DEFAULT = "0 0 2 * * ?";

	public static final int SE_DAGLIJST_OPHALEN_VOOR_DAGEN_DEFAULT = 1;

	private static final int MAMMA_SE_MAX_OFFLINE_INLOGPERIODE_DEFAULT = 0;

	private final SimplePreferenceService preferenceService;

	@Override
	public void voegParametersToe(SeAutorisatieDto result, String versie, MammaScreeningsEenheid screeningsEenheid)
	{
		var seMaxOfflineInlogPeriode = preferenceService.getInteger(PreferenceKey.MAMMA_SE_MAX_OFFLINE_INLOGPERIODE.name(), MAMMA_SE_MAX_OFFLINE_INLOGPERIODE_DEFAULT);

		Map<SeConfiguratieKey, String> seParameters = new EnumMap<>(SeConfiguratieKey.class);
		seParameters.put(SeConfiguratieKey.SE_MAX_OFFLINE_INLOG_PERIODE, Integer.toString(seMaxOfflineInlogPeriode));
		seParameters.put(SeConfiguratieKey.SE_DAGLIJST_OPHALEN_VOOR_DAGEN,
			Integer.toString(preferenceService.getInteger(PreferenceKey.MAMMA_SE_DAGLIJST_OPHALEN_DAGEN.name(), SE_DAGLIJST_OPHALEN_VOOR_DAGEN_DEFAULT)));
		seParameters.put(SeConfiguratieKey.SE_INFORMATIE_OPHALEN_CRON,
			preferenceService.getString(PreferenceKey.INTERNAL_MAMMA_SE_INFORMATIE_OPHALEN_CRON.name(), INTERNAL_MAMMA_SE_INFORMATIE_OPHALEN_CRON_DEFAULT));
		seParameters.put(SeConfiguratieKey.SE_PING_INTERVAL,
			Integer.toString(preferenceService.getInteger(PreferenceKey.INTERNAL_MAMMA_SE_PING_INTERVAL.name(), SE_PING_INTERVAL_DEFAULT)));
		seParameters.put(SeConfiguratieKey.SE_PONG_TIMEOUT,
			Integer.toString(preferenceService.getInteger(PreferenceKey.INTERNAL_MAMMA_SE_PONG_TIMEOUT.name(), SE_PONG_TIMEOUT_DEFAULT)));
		seParameters.put(SeConfiguratieKey.TOMOSYNTHESE_MOGELIJK, screeningsEenheid.getTomosyntheseMogelijk().toString());

		if (versie != null && !versie.startsWith("25.3")) 
		{
			seParameters.put(SeConfiguratieKey.IMS_LAUNCH_URL_PASSWORD, preferenceService.getString(PreferenceKey.MAMMA_IMS_LAUNCH_URL_PASSWORD.name(), ""));
			seParameters.put(SeConfiguratieKey.MAMMA_IMS_LAUNCH_URL_SHA1_MODE,
				String.valueOf(preferenceService.getBoolean(PreferenceKey.MAMMA_IMS_LAUNCH_URL_SHA1_MODE.name(), true)));
		}

		result.setSeParameters(seParameters);
	}
}
