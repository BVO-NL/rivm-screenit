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

import nl.rivm.screenit.model.colon.IFOBTResult;

import org.apache.commons.lang.StringUtils;

import ca.uhn.hl7v2.model.DataTypeException;
import ca.uhn.hl7v2.model.v251.datatype.NM;
import ca.uhn.hl7v2.model.v251.datatype.ST;
import ca.uhn.hl7v2.model.v251.message.OUL_R22;
import ca.uhn.hl7v2.model.v251.segment.MSH;

@Slf4j
@Getter
public class ColonIFobtHL7BerichtWrapper
{
	private final String labId;

	private final String messageId;

	private final List<IFOBTResult> results = new ArrayList<>();

	private final OUL_R22 message;

	public ColonIFobtHL7BerichtWrapper(OUL_R22 message) throws DataTypeException
	{
		this.message = message;
		MSH header = message.getMSH();
		messageId = header.getMsh10_MessageControlID().getValue();
		labId = header.getMsh4_SendingFacility().getHd1_NamespaceID().getValue();

		for (int i = 0; i < message.getSPECIMENReps(); i++)
		{
			var specimen = message.getSPECIMEN(i);
			var resultOBX = specimen.getORDER().getRESULT().getOBX();
			var resultSPM = specimen.getSPM();

			var ifobtResult = new IFOBTResult();

			var entityIdentifier = specimen.getCONTAINER().getSAC().getContainerIdentifier().getEntityIdentifier();

			if (entityIdentifier != null)
			{
				ifobtResult.setSid(entityIdentifier.getValue());
			}
			ifobtResult.setLabID(labId);

			var specimenRejectReason = resultSPM.getSpecimenRejectReason(0);

			var observationValue = resultOBX.getObx5_ObservationValue(0);
			var flag = resultOBX.getObx8_AbnormalFlags(0);

			var sid = ifobtResult.getSid();
			boolean isQC = sid != null && sid.startsWith("QC");

			if (isQC)
			{
				if (observationValue.getData() instanceof NM nm)
				{
					ifobtResult.setResultValue(nm.getValue());
				}
				else
				{

					throw new DataTypeException("Combinatie van SPM en OBX waardes klopt niet samen. Barcode: " + sid);
				}
			}
			else
			{

				if (specimenRejectReason.getCwe1_Identifier().getValue() != null)
				{
					ifobtResult.setOnbeoordeelbaarReden(specimenRejectReason.getCwe1_Identifier().getValue());
				}
				else if (observationValue.getData() instanceof ST st && flag.getValue() != null)
				{
					var flagValue = flag.getValue();

					if (!"PRO".equals(flagValue) && !"SS".equals(flagValue))
					{
						throw new DataTypeException("Combinatie van SPM en OBX waardes klopt niet samen. Barcode: " + sid);
					}
					var observationValueString = st.getValue();

					if (observationValueString == null || StringUtils.countMatches(observationValueString, "*") != observationValueString.length())
					{
						throw new DataTypeException("Combinatie van SPM en OBX waardes klopt niet samen. Barcode: " + sid);
					}
					ifobtResult.setFlag(flagValue);
				}

				else if (observationValue.getData() instanceof NM nm)
				{
					ifobtResult.setResultValue(nm.getValue());
				}
				else
				{
					throw new DataTypeException("Combinatie van SPM en OBX waardes klopt niet samen. Barcode: " + sid);
				}
			}

			var equipmentInstanceIdentifier = resultOBX.getObx18_EquipmentInstanceIdentifier(0).getEi1_EntityIdentifier();
			if (equipmentInstanceIdentifier != null)
			{
				ifobtResult.setInstrumentID(equipmentInstanceIdentifier.getValue());
			}

			try
			{
				var resultDate = resultOBX.getObx19_DateTimeOfTheAnalysis().getTime();
				if (resultDate != null)
				{
					ifobtResult.setDateTimeResult(resultDate.getValueAsDate());
				}
			}
			catch (DataTypeException ex)
			{
				LOG.warn("Fout bij parsen analysedatum FIT. MessageID: " + messageId);
				throw ex;
			}
			ifobtResult.setBestandsNaam(messageId);
			results.add(ifobtResult);
		}

	}
}
