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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.util.colon.ColonFitRegistratieUtil;

public enum ColonFitRegistratieStatus
{

	ACTIEF
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				if (!fitRegistratie.getType().equals(ColonFitType.STUDIE))
				{
					return Arrays.asList(VERLOREN, UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
				}
				else
				{
					return Arrays.asList(VERLOREN, UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN, VERWIJDERD).contains(nieuweStatus);
				}
			}
		},

	VERLOREN
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
			}
		},

	DOETNIETMEE
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
			}
		},

	WACHTOPTEST,

	WACHTOPTEST_VERWIJDERD_AF,

	WELBRIEFGEENTEST
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
			}
		},

	NIETONTVANGEN
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
			}
		},

	NIETTEBEOORDELEN
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(UITGEVOERD).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
			}
		},

	VERVALDATUMVERLOPEN
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(VERWIJDERD).contains(nieuweStatus);
			}
		},

	WACHTOPBRIEF,

	ONBETROUWBAAR,

	WELTESTGEENBRIEF
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(UITGEVOERD, VERVALDATUMVERLOPEN, NIETTEBEOORDELEN).contains(nieuweStatus) && uitgevoerdEnUitslag(nieuweStatus, fitRegistratie);
			}
		},

	UITGEVOERD
		{
			@Override
			public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
			{
				return Arrays.asList(VERWIJDERD).contains(nieuweStatus);
			}
		},

	VERWIJDERD,

	TB_PAIRED_WACHTOPTEST,

	TB_PAIRED_WACHTOPBRIEF,

	TB_P_TWEEDE_WACHTOPTEST,

	TB_P_TWEEDE_WACHTOPBRIEF,

	;

	private static final ColonFitRegistratieStatus[] MUTABLE_EIND_STATUSSEN = new ColonFitRegistratieStatus[] { VERVALDATUMVERLOPEN, NIETTEBEOORDELEN, UITGEVOERD, VERLOREN };

	public static final ColonFitRegistratieStatus[] UNMUTABLE_EIND_STATUSSEN = new ColonFitRegistratieStatus[] { VERWIJDERD };

	private static final List<ColonFitRegistratieStatus> STATUSSEN_MISLUKTE_ANALYSE = List.of(ColonFitRegistratieStatus.NIETTEBEOORDELEN,
		ColonFitRegistratieStatus.VERVALDATUMVERLOPEN);

	public boolean magWijzigenNaarStatus(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
	{
		return false;
	}

	private static boolean uitgevoerdEnUitslag(ColonFitRegistratieStatus nieuweStatus, ColonFitRegistratie fitRegistratie)
	{
		return nieuweStatus != UITGEVOERD || ColonFitRegistratieUtil.isOngunstig(fitRegistratie) || ColonFitRegistratieUtil.isGunstig(fitRegistratie);
	}

	public static boolean isMutableEindStatus(ColonFitRegistratieStatus status)
	{
		return Arrays.asList(MUTABLE_EIND_STATUSSEN).contains(status);
	}

	public static boolean isUnmutableEindStatus(ColonFitRegistratieStatus status)
	{
		return Arrays.asList(UNMUTABLE_EIND_STATUSSEN).contains(status);
	}

	public static boolean isMislukteAnalyse(ColonFitRegistratieStatus status)
	{
		return STATUSSEN_MISLUKTE_ANALYSE.contains(status);
	}

}
