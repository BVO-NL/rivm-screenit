package nl.rivm.screenit.model.colon.berichten;

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

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatDto;

import org.apache.commons.lang.StringUtils;

import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.v251.datatype.IS;
import ca.uhn.hl7v2.model.v251.datatype.NM;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.group.OUL_R22_SPECIMEN;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.model.v251.segment.OBX;

import static nl.rivm.screenit.util.colon.ColonFitRegistratieUtil.ANALYSE_RESULTAAT_FLAG_PRO;
import static nl.rivm.screenit.util.colon.ColonFitRegistratieUtil.ANALYSE_RESULTAAT_FLAG_SS;

@Slf4j
@Getter
public class ColonHl7BerichtToFitAnalyseResultaatSetWrapper
{
	private final String labId;

	private final String messageId;

	private final List<ColonFitAnalyseResultaatDto> results = new ArrayList<>();

	private final OUL_R22 message;

	public ColonHl7BerichtToFitAnalyseResultaatSetWrapper(OUL_R22 message) throws DataTypeException
	{
		this.message = message;
		var header = message.getMSH();
		messageId = header.getMsh10_MessageControlID().getValue();
		if (StringUtils.isBlank(messageId))
		{
			throw new DataTypeException("MSH.10: Geen messageId ingevuld.");
		}
		labId = header.getMsh4_SendingFacility().getHd1_NamespaceID().getValue();

		for (int i = 0; i < message.getSPECIMENReps(); i++)
		{
			var specimen = message.getSPECIMEN(i);
			var resultOBX = specimen.getORDER().getRESULT().getOBX();

			var analyseResultaatDto = new ColonFitAnalyseResultaatDto();

			var barcode = specimen.getCONTAINER().getSAC().getContainerIdentifier().getEntityIdentifier().getValue();
			if (StringUtils.isBlank(barcode))
			{
				throw new DataTypeException("SAC.3: Geen barcode in specimen: " + (i + 1));
			}
			analyseResultaatDto.setBarcode(barcode);
			analyseResultaatDto.setLabID(labId);

			boolean isQC = barcode.startsWith("QC");

			if (isQC)
			{
				kopieerQcAnalyseResultaat(resultOBX, analyseResultaatDto, barcode);
			}
			else
			{
				kopieerNormaleAnalyseResultaat(specimen, analyseResultaatDto, barcode);
			}
			kopieerInstrumentId(resultOBX, analyseResultaatDto, barcode);
			kopieerAnalyseDatum(resultOBX, analyseResultaatDto, barcode);

			analyseResultaatDto.setBestandsNaam(messageId);
			results.add(analyseResultaatDto);
		}

	}

	private static void kopieerQcAnalyseResultaat(OBX obx, ColonFitAnalyseResultaatDto analyseResultaatDto, String barcode) throws DataTypeException
	{
		var observationValue = obx.getObx5_ObservationValue(0);
		if (observationValue.getData() instanceof NM nm && StringUtils.isNotBlank(nm.getValue()))
		{
			analyseResultaatDto.setResultValue(nm.getValue());
		}
		else
		{

			throw new DataTypeException("OBX.2: Geen analyse resultaat. Barcode: " + barcode);
		}
	}

	private static void kopieerNormaleAnalyseResultaat(OUL_R22_SPECIMEN specimen, ColonFitAnalyseResultaatDto analyseResultaatDto, String barcode)
		throws DataTypeException
	{
		var resultOBX = specimen.getORDER().getRESULT().getOBX();
		var resultSPM = specimen.getSPM();
		var specimenRejectReason = resultSPM.getSpecimenRejectReason(0);

		var observationValue = resultOBX.getObx5_ObservationValue(0);
		var flag = resultOBX.getObx8_AbnormalFlags(0);

		var onbeoordeelbaarReden = specimenRejectReason.getCwe1_Identifier().getValue();
		if (StringUtils.isNotBlank(onbeoordeelbaarReden))
		{
			if (StringUtils.isNumeric(onbeoordeelbaarReden))
			{
				analyseResultaatDto.setOnbeoordeelbaarReden(onbeoordeelbaarReden);
			}
			else
			{
				throw new DataTypeException("SPM.21: Waarde (" + onbeoordeelbaarReden + ") is niet numeriek. Barcode: " + barcode);
			}
		}
		else if (observationValue.getData() instanceof ST st && StringUtils.isNotBlank(flag.getValue()))
		{
			kopieerFlag(analyseResultaatDto, barcode, st, flag);
		}

		else if (observationValue.getData() instanceof NM nm && StringUtils.isNotBlank(nm.getValue()))
		{
			analyseResultaatDto.setResultValue(nm.getValue());
		}
		else
		{
			throw new DataTypeException("Combinatie van SPM en OBX waardes klopt niet samen. Barcode: " + barcode);
		}
	}

	private static void kopieerFlag(ColonFitAnalyseResultaatDto analyseResultaatDto, String barcode, ST st, IS flag) throws DataTypeException
	{
		var flagValue = flag.getValue();

		if (!ANALYSE_RESULTAAT_FLAG_PRO.equals(flagValue) && !ANALYSE_RESULTAAT_FLAG_SS.equals(flagValue))
		{
			throw new DataTypeException("OBX.5: Onbekende flag: " + flagValue + ". Barcode: " + barcode);
		}
		var observationValueString = st.getValue();

		if (observationValueString == null || StringUtils.countMatches(observationValueString, "*") != observationValueString.length())
		{
			throw new DataTypeException("Combinatie van SPM en OBX waardes klopt niet samen. Barcode: " + barcode);
		}
		analyseResultaatDto.setFlag(flagValue);
	}

	private static void kopieerInstrumentId(OBX obx, ColonFitAnalyseResultaatDto analyseResultaatDto, String barcode) throws DataTypeException
	{
		var equipmentInstanceIdentifier = obx.getObx18_EquipmentInstanceIdentifier(0).getEi1_EntityIdentifier().getValue();
		if (StringUtils.isNotBlank(equipmentInstanceIdentifier))
		{
			analyseResultaatDto.setInstrumentID(equipmentInstanceIdentifier);
		}
		else if (heeftAnalyseResultaat(analyseResultaatDto))
		{
			throw new DataTypeException("OBX.18: Geen instrument identifier bij analyse resultaat. Barcode: " + barcode);
		}
	}

	private void kopieerAnalyseDatum(OBX resultOBX, ColonFitAnalyseResultaatDto analyseResultaatDto, String barcode) throws DataTypeException
	{
		try
		{
			var resultDate = resultOBX.getObx19_DateTimeOfTheAnalysis().getTime().getValueAsDate();
			if (resultDate != null)
			{
				analyseResultaatDto.setDateTimeResult(resultDate);
			}
			else if (heeftAnalyseResultaat(analyseResultaatDto))
			{
				throw new DataTypeException("OBX.19: Geen analysedatum. Barcode: " + barcode);
			}
		}
		catch (DataTypeException ex)
		{
			LOG.warn("Fout bij parsen analysedatum FIT. MessageID: {}", messageId);
			throw ex;
		}
	}

	private static boolean heeftAnalyseResultaat(ColonFitAnalyseResultaatDto analyseResultaatDto)
	{
		return analyseResultaatDto.getResultValue() != null || ANALYSE_RESULTAAT_FLAG_PRO.equals(analyseResultaatDto.getFlag());
	}

}
