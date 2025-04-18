package nl.rivm.screenit.service;

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

import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging;

public interface BaseUitnodigingService
{
	<U extends Uitnodiging<?>> boolean heeftAlEenNieuwereUitnodiging(U huidigeUitnodiging);

	<U extends InpakbareUitnodiging<?>> boolean isVerstuurdMetTijdelijkAdres(U uitnodiging);

	<U extends InpakbareUitnodiging<?>> boolean isAdresGewijzigdNaUitnodigingsdatum(U uitnodiging);

	ColonUitnodiging getColonUitnodiging(String trackId, String postcode, Integer huisnummer);

	CervixUitnodiging getCervixUitnodiging(String trackId, String postcode, Integer huisnummer);

	boolean colonUitnodigingExists(String trackId);

	boolean cervixUitnodigingExists(String trackId);
}
