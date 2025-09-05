package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bmhk
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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.cervix.CervixUitnodigingRepository;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Service;

import generated.KOPPELDATA;

import static nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification.heeftUitnodigingId;

@Service
@AllArgsConstructor
public class BarcodeZasValiderenServiceImpl extends BaseValiderenService implements BarcodeValiderenService
{
	private final BaseHoudbaarheidService houdbaarheidService;

	private final CervixBaseMonsterService monsterService;

	private final HibernateService hibernateService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final CervixUitnodigingRepository uitnodigingRepository;

	@Override
	public List<String> voerSemantischeValidatieUit(List<KOPPELDATA.VERZONDENUITNODIGING> koppeldata)
	{
		List<String> foutmeldingen = new ArrayList<>();
		List<String> barcodesUitKoppeldata = new ArrayList<>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMetCervix = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.PERIODE_MINIMALE_HOUDBAARHEID_ZAS_MONSTERS_VOOR_CONTROLE);

		for (var verzondenUitnodiging : koppeldata)
		{

			var zasBarcode = getMatchingFieldValue(verzondenUitnodiging, KoppelConstants.CERVIX_KOPPEL_BARCODE_ZAS);
			var cervixUitnodiging = getCervixUitnodiging(verzondenUitnodiging);

			valideer(foutmeldingen, cervixUitnodiging, verzondenUitnodiging.getID(), zasBarcode, minstensHoudbaarTotMetCervix, barcodesUitKoppeldata);
		}

		return foutmeldingen;
	}

	@Override
	public List<String> valideerOpSemantiek(List<InpakcentrumKoppelDataDto> koppeldata)
	{
		List<String> foutmeldingen = new ArrayList<>();
		List<String> barcodesUitKoppeldata = new ArrayList<>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMetCervix = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.PERIODE_MINIMALE_HOUDBAARHEID_ZAS_MONSTERS_VOOR_CONTROLE);

		for (var koppelDataDto : koppeldata)
		{

			var zasBarcode = koppelDataDto.getBarcode();
			var uitnodiging = uitnodigingRepository.findOne(heeftUitnodigingId(koppelDataDto.getId())).orElse(null);

			valideer(foutmeldingen, uitnodiging, koppelDataDto.getId(), zasBarcode, minstensHoudbaarTotMetCervix, barcodesUitKoppeldata);
		}
		return foutmeldingen;
	}

	private void valideer(List<String> foutmeldingen, CervixUitnodiging uitnodiging, Long uitnodigingId, String zasBarcode, LocalDate minstensHoudbaarTotMetCervix,
		List<String> barcodesUitKoppeldata)
	{
		if (uitnodiging == null)
		{
			addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_UITNODIGINGSID_ONBEKEND, uitnodigingId, zasBarcode));
		}
		else
		{
			if (StringUtils.isBlank(zasBarcode))
			{
				addFout(foutmeldingen,
					String.format(KoppelConstants.CERVIX_ZASID_MIST_BIJ_TYPE_UITNODIGING, uitnodigingId,
						StringUtils.defaultIfBlank(zasBarcode, KoppelConstants.DEFAULT_STRING)));
			}
			if (StringUtils.isNotBlank(zasBarcode))
			{
				var bestaandeZas = monsterService.getZas(zasBarcode).orElse(null);
				if (bestaandeZas != null && !bestaandeZas.getUitnodiging().equals(uitnodiging))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_ZASID_AL_GEKOPPELD, uitnodigingId,
						StringUtils.defaultIfBlank(zasBarcode, KoppelConstants.DEFAULT_STRING), bestaandeZas.getUitnodiging().getUitnodigingsId()));
				}
				if (uitnodiging.getMonster() != null && !uitnodiging.getMonster().getMonsterId().equals(zasBarcode))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_UITNODIGINGSID_AL_GEKOPPELD, uitnodigingId, zasBarcode,
						uitnodiging.getMonster().getMonsterId()));
				}
				var houdbaarheid = houdbaarheidService.getZasHoudbaarheidVoor(zasBarcode);
				if (houdbaarheid == null)
				{
					addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_HOUDBAARHEID_ONBEKEND, uitnodigingId,
						StringUtils.defaultIfBlank(zasBarcode, KoppelConstants.DEFAULT_STRING)));
				}
				else if (DateUtil.toLocalDate(houdbaarheid.getVervalDatum()).isBefore(minstensHoudbaarTotMetCervix))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_HOUDBAARHEID_TE_KORT, uitnodigingId,
						StringUtils.defaultIfBlank(zasBarcode, KoppelConstants.DEFAULT_STRING)));
				}
				if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, zasBarcode))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.CERVIX_ZASID_IS_DUBBEL, uitnodigingId, zasBarcode));
				}
				else
				{
					barcodesUitKoppeldata.add(zasBarcode);
				}
			}
		}
	}

	private CervixUitnodiging getCervixUitnodiging(KOPPELDATA.VERZONDENUITNODIGING verzondenUitnodiging)
	{
		var parameters = new HashMap<String, Long>();
		parameters.put("uitnodigingsId", verzondenUitnodiging.getID());
		return hibernateService.getUniqueByParameters(CervixUitnodiging.class, parameters);
	}

	@Override
	protected void logKoppelenFout(LogEvent logEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_ZAS_CONTROLE_KOPPELEN_FOUT, logEvent, Bevolkingsonderzoek.CERVIX);
	}
}
