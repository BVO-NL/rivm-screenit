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

import java.time.LocalDate;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.cervix.CervixHoudbaarheidZasReeks;
import nl.rivm.screenit.model.colon.ColonHoudbaarheidFitReeks;
import nl.rivm.screenit.repository.cervix.CervixHoudbaarheidZasReeksRepository;
import nl.rivm.screenit.repository.colon.ColonHoudbaarheidFitReeksRepository;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.cervix.CervixZasHoudbaarheidSpecification;
import nl.rivm.screenit.specification.colon.ColonHoudbaarheidFitReeksSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class BaseHoudbaarheidServiceImpl implements BaseHoudbaarheidService
{
	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private CervixHoudbaarheidZasReeksRepository houdbaarheidZasReeksRepository;

	@Autowired
	private ColonHoudbaarheidFitReeksRepository houdbaarheidFitReeksRepository;

	@Override
	public boolean isZasHoudbaar(String barcode)
	{
		var houdbaarheid = getZasHoudbaarheidVoor(barcode);
		var nu = currentDateSupplier.getDate();
		return houdbaarheid != null && houdbaarheid.getVervalDatum().after(nu);
	}

	@Override
	public boolean isFitHoudbaar(String barcode)
	{
		var houdbaarheid = getFitHoudbaarheidVoor(barcode);
		var nu = currentDateSupplier.getDate();
		return houdbaarheid != null && !houdbaarheid.getVervalDatum().before(nu);
	}

	@Override
	public CervixHoudbaarheidZasReeks getZasHoudbaarheidVoor(String barcode)
	{
		return houdbaarheidZasReeksRepository.findOne(CervixZasHoudbaarheidSpecification.heeftBarcodeInRange(barcode)).orElse(null);
	}

	@Override
	public ColonHoudbaarheidFitReeks getFitHoudbaarheidVoor(String barcode)
	{
		return houdbaarheidFitReeksRepository.findOne(ColonHoudbaarheidFitReeksSpecification.heeftBarcodeInRange(barcode)).orElse(null);
	}

	@Override
	public LocalDate getMinstensHoudbaarTotMet(LocalDate vandaag, PreferenceKey preferenceKey)
	{
		var periode = simplePreferenceService.getInteger(preferenceKey.name());
		if (periode == null)
		{
			periode = 61;
		}
		else
		{
			periode++;
		}
		return vandaag.plusDays(periode);
	}
}
