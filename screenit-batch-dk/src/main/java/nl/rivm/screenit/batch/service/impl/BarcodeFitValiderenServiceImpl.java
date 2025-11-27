package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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
import java.util.List;

import lombok.Setter;

import nl.rivm.screenit.KoppelConstants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.service.BarcodeValiderenService;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.inpakcentrum.vaninpakcentrum.InpakcentrumKoppelDataDto;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.service.BaseHoudbaarheidService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.colon.ColonBaseFitService;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import generated.KOPPELDATA;

import static nl.rivm.screenit.specification.algemeen.InpakbareUitnodigingSpecification.heeftUitnodigingId;

@Setter
@Service
public class BarcodeFitValiderenServiceImpl extends BaseValiderenService implements BarcodeValiderenService
{
	@Autowired
	private BaseHoudbaarheidService houdbaarheidService;

	@Autowired
	private ColonBaseFitService fitService;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Autowired
	private ColonUitnodigingRepository uitnodigingRepository;

	@Override
	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	public List<String> voerSemantischeValidatieUit(List<KOPPELDATA.VERZONDENUITNODIGING> koppeldata)
	{
		var foutmeldingen = new ArrayList<String>();
		var barcodesUitKoppeldata = new ArrayList<String>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMet = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.COLON_PERIODE_MINIMALE_HOUDBAARHEID_FIT_MONSTERS_VOOR_CONTROLE);

		koppeldata.forEach(koppelRegel -> valideerKoppelRegel(koppelRegel, foutmeldingen, minstensHoudbaarTotMet, barcodesUitKoppeldata));
		return foutmeldingen;
	}

	@Override
	public List<String> valideerOpSemantiek(List<InpakcentrumKoppelDataDto> koppeldata)
	{
		var foutmeldingen = new ArrayList<String>();
		var barcodesUitKoppeldata = new ArrayList<String>();

		var vandaag = currentDateSupplier.getLocalDate();
		var minstensHoudbaarTotMet = houdbaarheidService.getMinstensHoudbaarTotMet(vandaag, PreferenceKey.COLON_PERIODE_MINIMALE_HOUDBAARHEID_FIT_MONSTERS_VOOR_CONTROLE);

		koppeldata.forEach(koppelRegel -> valideerKoppelData(koppelRegel, foutmeldingen, minstensHoudbaarTotMet, barcodesUitKoppeldata));
		return foutmeldingen;
	}

	private void valideerKoppelData(InpakcentrumKoppelDataDto koppelDataDto, List<String> foutmeldingen, LocalDate minstensHoudbaarTotMetColon,
		List<String> barcodesUitKoppeldata)
	{
		var uitnodiging = getUitnodiging(koppelDataDto.getId());
		var fitBarcodeGold = koppelDataDto.getBarcode();
		var fitBarcodeExtra = koppelDataDto.getBarcodeExtra();

		valideer(foutmeldingen, uitnodiging, koppelDataDto.getId(), fitBarcodeGold, fitBarcodeExtra, minstensHoudbaarTotMetColon, barcodesUitKoppeldata);
	}

	@Deprecated(forRemoval = true, since = "nieuwe endpoint wordt gebruikt in PROD")
	private void valideerKoppelRegel(KOPPELDATA.VERZONDENUITNODIGING koppelRegel, List<String> foutmeldingen, LocalDate minstensHoudbaarTotMetColon,
		List<String> barcodesUitKoppeldata)
	{
		var uitnodiging = getUitnodiging(koppelRegel.getID());
		var fitBarcodeGold = getMatchingFieldValue(koppelRegel, KoppelConstants.COLON_KOPPEL_BARCODE_GOLD);
		var fitBarcodeExtra = getMatchingFieldValue(koppelRegel, KoppelConstants.COLON_KOPPEL_BARCODE_EXTRA);

		valideer(foutmeldingen, uitnodiging, koppelRegel.getID(), fitBarcodeGold, fitBarcodeExtra, minstensHoudbaarTotMetColon, barcodesUitKoppeldata);
	}

	private void valideer(List<String> foutmeldingen, ColonUitnodiging uitnodiging, Long uitnodigingId, String fitBarcodeGold, String fitBarcodeExtra,
		LocalDate minstensHoudbaarTotMetColon, List<String> barcodesUitKoppeldata)
	{
		if (uitnodiging == null)
		{
			addFout(foutmeldingen, String.format(KoppelConstants.COLON_UITNODIGINGSID_ONBEKEND, uitnodigingId, fitBarcodeGold,
				StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING)));
		}
		else
		{
			var onderzoeksvariant = getOnderzoeksVariant(uitnodiging);
			var barcodeGoldVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksvariant, ColonFitType.GOLD);
			var barcodeExtraVerplicht = ColonOnderzoeksVariant.isOfType(onderzoeksvariant, ColonFitType.STUDIE);

			if (barcodeGoldVerplicht && StringUtils.isBlank(fitBarcodeGold))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING, uitnodigingId,
					StringUtils.defaultIfBlank(fitBarcodeGold, KoppelConstants.DEFAULT_STRING), StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), ""));
			}
			if (barcodeExtraVerplicht && StringUtils.isBlank(fitBarcodeExtra))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING, uitnodigingId,
					StringUtils.defaultIfBlank(fitBarcodeGold, KoppelConstants.DEFAULT_STRING), StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING),
					"Extra"));
			}
			if (!barcodeGoldVerplicht && StringUtils.isNotBlank(fitBarcodeGold))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING, uitnodigingId,
					StringUtils.defaultIfBlank(fitBarcodeGold, KoppelConstants.DEFAULT_STRING), StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), ""));
			}
			if (!barcodeExtraVerplicht && StringUtils.isNotBlank(fitBarcodeExtra))
			{
				addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING, uitnodigingId,
					StringUtils.defaultIfBlank(fitBarcodeGold, KoppelConstants.DEFAULT_STRING), StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING),
					"Extra"));
			}
			if (StringUtils.isNotBlank(fitBarcodeGold))
			{
				fitService.getFit(fitBarcodeGold).ifPresent(bestaandeFitGold ->
				{
					if (ColonFitType.GOLD.equals(bestaandeFitGold.getType())
						&& !bestaandeFitGold.getUitnodiging().equals(uitnodiging))
					{
						addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_AL_GEKOPPELD, uitnodigingId,
							StringUtils.defaultIfBlank(fitBarcodeGold, KoppelConstants.DEFAULT_STRING),
							StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), "",
							bestaandeFitGold.getUitnodiging().getUitnodigingsId()));
					}
				});
				if (uitnodiging.getGekoppeldeFitRegistratie() != null && !uitnodiging.getGekoppeldeFitRegistratie().getBarcode().equals(fitBarcodeGold))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_UITNODIGINGSID_AL_GEKOPPELD, uitnodigingId, fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), "", uitnodiging.getGekoppeldeFitRegistratie().getBarcode()));
				}
				var fitHoudbaarheid = houdbaarheidService.getFitHoudbaarheidVoor(fitBarcodeGold);
				if (fitHoudbaarheid == null)
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_ONBEKEND, uitnodigingId, fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), ""));
				}
				else if (DateUtil.toLocalDate(fitHoudbaarheid.getVervalDatum()).isBefore(minstensHoudbaarTotMetColon))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_TE_KORT, uitnodigingId, fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), ""));
				}
				if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, fitBarcodeGold))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_BUISID_IS_DUBBEL, uitnodigingId, fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING)));
				}
				else
				{
					barcodesUitKoppeldata.add(fitBarcodeGold);
				}
			}
			if (StringUtils.isNotBlank(fitBarcodeExtra))
			{
				var bestaandeFitExtra = fitService.getFit(fitBarcodeExtra).orElse(null);
				if (bestaandeFitExtra != null && ColonFitType.STUDIE.equals(bestaandeFitExtra.getType())
					&& !bestaandeFitExtra.getUitnodigingExtra().equals(uitnodiging))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.COLON_BUISID_AL_GEKOPPELD, uitnodigingId,
							StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING),
							StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), "Extra ",
							bestaandeFitExtra.getUitnodiging().getUitnodigingsId()));
				}
				if (uitnodiging.getGekoppeldeExtraFitRegistratie() != null && !uitnodiging.getGekoppeldeExtraFitRegistratie().getBarcode().equals(fitBarcodeExtra))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.COLON_UITNODIGINGSID_AL_GEKOPPELD, uitnodigingId, fitBarcodeGold,
							StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), "Extra ", uitnodiging.getGekoppeldeExtraFitRegistratie().getBarcode()));
				}
				var fitHoudbaarheid = houdbaarheidService.getFitHoudbaarheidVoor(fitBarcodeExtra);
				if (fitHoudbaarheid == null)
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_ONBEKEND, uitnodigingId, fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), "Extra "));
				}
				else if (DateUtil.toLocalDate(fitHoudbaarheid.getVervalDatum()).isBefore(minstensHoudbaarTotMetColon))
				{
					addFout(foutmeldingen, String.format(KoppelConstants.COLON_HOUDBAARHEID_TE_KORT, uitnodigingId, fitBarcodeGold,
						StringUtils.defaultIfBlank(fitBarcodeExtra, KoppelConstants.DEFAULT_STRING), "Extra "));
				}
				if (barcodeAlTeruggekoppeld(barcodesUitKoppeldata, fitBarcodeExtra))
				{
					addFout(foutmeldingen,
						String.format(KoppelConstants.COLON_BUISID_IS_DUBBEL, uitnodigingId, fitBarcodeGold, fitBarcodeExtra));
				}
				else
				{
					barcodesUitKoppeldata.add(fitBarcodeExtra);
				}
			}
		}
	}

	private ColonOnderzoeksVariant getOnderzoeksVariant(ColonUitnodiging uitnodiging)
	{
		ColonOnderzoeksVariant onderzoeksVariant = null;
		if (uitnodiging != null)
		{
			onderzoeksVariant = uitnodiging.getOnderzoeksVariant();
		}
		return onderzoeksVariant;
	}

	private ColonUitnodiging getUitnodiging(Long uitnodigingId)
	{
		return uitnodigingRepository.findOne(heeftUitnodigingId(uitnodigingId)).orElse(null);
	}

	@Override
	protected void logKoppelenFout(LogEvent logEvent)
	{
		logService.logGebeurtenis(LogGebeurtenis.COLON_JOB_CONTROLE_FIT_KOPPELEN_FOUT, logEvent, Bevolkingsonderzoek.COLON);
	}

}
