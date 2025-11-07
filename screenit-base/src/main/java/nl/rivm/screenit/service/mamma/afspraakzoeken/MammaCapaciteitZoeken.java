package nl.rivm.screenit.service.mamma.afspraakzoeken;

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

import java.util.Collections;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaCapaciteitZoeken
{
	public static <R extends MammaRationaal> R elementMetRelatiefMeesteVrijeCapaciteit(List<R> list)
	{
		if (list.size() > 2)
		{
			var middle = list.size() / 2;

			if (list.get(0) instanceof MammaAfspraakOptie)
			{
				var afspraakOpties = list.stream().map(MammaAfspraakOptie.class::cast).toList();
				var midden = gecorrigeerdMidden(afspraakOpties, middle);
				if (midden != null)
				{
					middle = midden;
				}
				else
				{
					return list.get(list.size() - 1);
				}
			}

			var list1 = list.subList(0, middle);
			var list2 = list.subList(middle, list.size());

			var rationaal1 = MammaRationaal.getRationaal(list1);
			var rationaal2 = MammaRationaal.getRationaal(list2);

			if (rationaal1.compareTo(rationaal2) <= 0)
			{
				return elementMetRelatiefMeesteVrijeCapaciteit(list1);
			}
			else
			{
				return elementMetRelatiefMeesteVrijeCapaciteit(list2);
			}
		}
		else
		{
			return Collections.min(list);
		}
	}

	private static Integer gecorrigeerdMidden(List<MammaAfspraakOptie> afspraken, int middle)
	{
		var vanafMiddle = afspraken.get(middle).getVanaf();
		var index = middle - 1;
		var forward = true;
		for (var i = 2; 0 <= index && index < afspraken.size(); i++)
		{
			var vanafIndex = afspraken.get(index).getVanaf();
			if (!vanafMiddle.equals(vanafIndex))
			{
				return forward ? index + 1 : index;
			}
			index = forward ? index + i : index - i;
			forward = !forward;
		}
		return null;
	}
}
