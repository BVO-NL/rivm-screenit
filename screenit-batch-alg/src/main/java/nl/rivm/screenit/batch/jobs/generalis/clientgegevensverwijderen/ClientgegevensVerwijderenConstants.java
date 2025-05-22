package nl.rivm.screenit.batch.jobs.generalis.clientgegevensverwijderen;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ClientgegevensVerwijderenConstants
{
	public static final String TOTAAL_AANTAL_CLIENTEN_NAW_KEY = "clientgegevensverwijderen.totaal.aantal.clienten.naw";

	public static final String TOTAAL_AANTAL_CLIENTEN_REST_KEY = "clientgegevensverwijderen.totaal.aantal.clienten.rest";

	public static final String TOTAAL_AANTAL_CLIENTEN_OUD_KEY = "clientgegevensverwijderen.totaal.aantal.clienten.oud";

	public static final String TOTAAL_AANTAL_PERSOONSGEGEVENS_CLIENTEN_NIET_VERWIJDERD_KEY = "clientgegevensverwijderen.totaal.aantal.persoonsgegevens.clienten.niet.verwijderd";

	public static final String TOTAAL_AANTAL_CLIENTEN_NIET_VERWIJDERD_KEY = "clientgegevensverwijderen.totaal.aantal.clienten.niet.verwijderd";

}
