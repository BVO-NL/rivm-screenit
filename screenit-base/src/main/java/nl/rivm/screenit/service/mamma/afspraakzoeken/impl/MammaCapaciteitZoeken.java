package nl.rivm.screenit.service.mamma.afspraakzoeken.impl;

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

import nl.rivm.screenit.service.mamma.afspraakzoeken.MammaAfspraakOptie;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
class MammaCapaciteitZoeken
{
	static <R extends MammaRationaal> R elementMetRelatiefMeesteVrijeCapaciteit(List<R> rationalen)
	{
		if (rationalen.size() > 2)
		{
			var midden = rationalen.size() / 2;

			if (rationalen.get(0) instanceof MammaAfspraakOptie)
			{
				var afspraakOpties = rationalen.stream().map(MammaAfspraakOptie.class::cast).toList();
				var gecorrigeerdMidden = gecorrigeerdMidden(afspraakOpties, midden);
				if (gecorrigeerdMidden != null)
				{
					midden = gecorrigeerdMidden;
				}
				else
				{
					return rationalen.get(rationalen.size() - 1);
				}
			}

			var eersteHelft = rationalen.subList(0, midden);
			var tweedeHelft = rationalen.subList(midden, rationalen.size());

			var gecombineerdeRationaalEersteHelft = MammaRationaal.gecombineerdeRationaal(eersteHelft);
			var gecombineerdeRationaalTweedeHelft = MammaRationaal.gecombineerdeRationaal(tweedeHelft);

			if (gecombineerdeRationaalEersteHelft.compareTo(gecombineerdeRationaalTweedeHelft) <= 0)
			{
				return elementMetRelatiefMeesteVrijeCapaciteit(eersteHelft);
			}
			else
			{
				return elementMetRelatiefMeesteVrijeCapaciteit(tweedeHelft);
			}
		}
		else
		{
			return Collections.min(rationalen);
		}
	}

	private static Integer gecorrigeerdMidden(List<MammaAfspraakOptie> afspraken, int middle)
	{
		var vanafMiddle = afspraken.get(middle).getTijd();
		var index = middle - 1;
		var forward = true;
		for (var i = 2; 0 <= index && index < afspraken.size(); i++)
		{
			var vanafIndex = afspraken.get(index).getTijd();
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
