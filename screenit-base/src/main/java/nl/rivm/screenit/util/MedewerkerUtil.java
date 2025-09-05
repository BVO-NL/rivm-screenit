package nl.rivm.screenit.util;

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

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.model.Medewerker;
import nl.rivm.screenit.model.OrganisatieMedewerker;

import org.apache.commons.lang3.BooleanUtils;
import org.hibernate.Session;

public class MedewerkerUtil
{

	private MedewerkerUtil()
	{
	}

	public static String getMedewerkerNamen(List<OrganisatieMedewerker> organisatieMedewerkers)
	{
		return organisatieMedewerkers.stream()
			.filter(om -> om != null && om.getMedewerker() != null)
			.map(om -> om.getMedewerker().getNaamVolledig())
			.collect(Collectors.joining(", "));
	}

	public static boolean isMedewerkerActief(Medewerker medewerker, Date now)
	{
		return BooleanUtils.isNotFalse(medewerker.getActief())
			&& (medewerker.getActiefVanaf() == null || !DateUtil.compareAfter(medewerker.getActiefVanaf(), now))
			&& (medewerker.getActiefTotEnMet() == null || !DateUtil.compareBefore(medewerker.getActiefTotEnMet(), now));
	}

	public static String meldingNavActiefVanafEnTotEnMet(Medewerker medewerker, final Date now)
	{
		if (medewerker != null && medewerker.getActiefTotEnMet() != null && DateUtil.compareBefore(medewerker.getActiefTotEnMet(), now))
		{
			return "Uw account is geïnactiveerd";
		}
		else if (medewerker != null && medewerker.getActiefVanaf() != null && DateUtil.compareAfter(medewerker.getActiefVanaf(), now))
		{
			return "Uw account is nog niet actief";
		}
		else
		{
			return null;
		}
	}

	public static int getNextMedewerkercode(Session session)
	{
		return session.doReturningWork(new SequenceGenerator(DatabaseSequence.MEDEWERKERCODE, session.getSessionFactory())).intValue();
	}
}
