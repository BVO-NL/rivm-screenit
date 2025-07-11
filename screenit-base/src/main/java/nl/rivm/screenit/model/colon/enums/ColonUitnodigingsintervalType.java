package nl.rivm.screenit.model.colon.enums;

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

import java.util.List;

import org.apache.commons.lang3.StringUtils;

public enum ColonUitnodigingsintervalType
{
	UITNODIGING_ONTVANGEN(ColonPeildatumSoort.DATUM_START_RONDE),
	VERWIJDERD_DOSSIER(ColonPeildatumSoort.DATUM_START_RONDE),
	GUNSTIGE_UITSLAG(ColonPeildatumSoort.DATUM_START_RONDE),
	OPEN_UITNODIGING(ColonPeildatumSoort.DATUM_START_RONDE),
	GEPLANDE_INTAKE_AFSPRAAK(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	GEANNULEERDE_INTAKE_AFSPRAAK(ColonPeildatumSoort.DATUM_START_RONDE),
	INTAKE_COLOSCOPIE_GEPLAND(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_CLIENT_WIL_ANDERE_LOCATIE(ColonPeildatumSoort.DATUM_START_RONDE),
	INTAKE_ON_HOLD(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_CT_COLOGRAFIE(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_NO_SHOW(ColonPeildatumSoort.DATUM_START_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_VERZOEK_CLIENT(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_MEDISCHE_REDENEN(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_2_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_3_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_4_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_5_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_6_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_7_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_8_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_9_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	INTAKE_GEEN_VERVOLGBELEID_10_JAAR_TERUG_NAAR_SCREENING(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_VERWIJZING_POLIKLINIEK(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_VERVOLGSCOPIE(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SCOPIE_BEOORDELING_RADICALITEIT(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_NIEUWE_SCOPIE(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_3_JAAR(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_5_JAAR(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_6_MND(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_1_JAAR(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_2_JAAR(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_3_JAAR(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_5_JAAR_EXCLUSIE(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_TERUG_BVO(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_CT_COLOGRAFIE(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE),
	ENDOSCOPIEVERSLAG_GEEN_SURVEILLANCE(ColonPeildatumSoort.EERSTE_ONGUNSTIGE_UITSLAG_VORIGE_RONDE);

	private final ColonPeildatumSoort peildatumSoort;

	private ColonUitnodigingsintervalType(ColonPeildatumSoort peildatumSoort)
	{
		this.peildatumSoort = peildatumSoort;
	}

	public ColonPeildatumSoort peildatumSoort()
	{
		return peildatumSoort;
	}

	public String naam()
	{
		return StringUtils.capitalize(name().toLowerCase().replace("_", " "));
	}

	public static final List<ColonUitnodigingsintervalType> intervalTypesMetVooraankondiging = List.of(ColonUitnodigingsintervalType.INTAKE_COLOSCOPIE_GEPLAND,
		INTAKE_CT_COLOGRAFIE,
		INTAKE_GEEN_VERVOLGBELEID_3_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_4_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_5_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_6_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_7_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_8_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_9_JAAR_TERUG_NAAR_SCREENING,
		INTAKE_GEEN_VERVOLGBELEID_10_JAAR_TERUG_NAAR_SCREENING,
		ENDOSCOPIEVERSLAG_VERWIJZING_POLIKLINIEK,
		ENDOSCOPIEVERSLAG_VERVOLGSCOPIE,
		ENDOSCOPIEVERSLAG_SCOPIE_BEOORDELING_RADICALITEIT,
		ENDOSCOPIEVERSLAG_NIEUWE_SCOPIE,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_1_JAAR,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_3_JAAR,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_5_JAAR,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_6_MND,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_1_JAAR,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_2_JAAR,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_3_JAAR,
		ENDOSCOPIEVERSLAG_SURVEILLANCE_2025_5_JAAR_EXCLUSIE,
		ENDOSCOPIEVERSLAG_TERUG_BVO,
		ENDOSCOPIEVERSLAG_CT_COLOGRAFIE,
		ENDOSCOPIEVERSLAG_GEEN_SURVEILLANCE);
}
