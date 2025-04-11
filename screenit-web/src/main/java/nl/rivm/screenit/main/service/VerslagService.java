package nl.rivm.screenit.main.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.model.BerichtZoekFilter;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.berichten.Verslag;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.colon.MdlVerslag;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;

public interface VerslagService
{
	List<MdlVerslag> getAlleMdlVerslagenVanClient(Client client);

	List<OntvangenCdaBericht> zoekBerichten(BerichtZoekFilter filter, long first, long count, String property, boolean ascending);

	long countBerichten(BerichtZoekFilter filter);

	<V extends Verslag<?, ?>> V heropenVerslag(V verslag, InstellingGebruiker instellingGebruiker);

	void herverwerkAlleBerichten(BerichtZoekFilter nullSafeGet);

	<V extends Verslag<?, ?>> List<V> zoekVerslagen(V zoekObject, int first, int count, String property, boolean ascending);

	<V extends Verslag<?, ?>> long countVerslagen(V zoekObject);

	void berichtenOpnieuwVerwerken(List<Long> ids, Bevolkingsonderzoek bvo);

	void berichtOpnieuwVerwerken(OntvangenCdaBericht ontvangenCdaBericht);
}
