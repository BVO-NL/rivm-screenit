package nl.rivm.screenit;

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

public class KoppelConstants
{

	public static final String COLON_UITNODIGINGSID_ONBEKEND = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: UitnodigingsID is niet bekend";

	public static final String COLON_BUISID_AL_GEKOPPELD = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: %4$sBuisID is al gekoppeld aan ander UitnodigingsId %5$s";

	public static final String COLON_UITNODIGINGSID_AL_GEKOPPELD = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: UitnodigingsId is al gekoppeld aan ander %4$sBuisID %5$s";

	public static final String COLON_HOUDBAARHEID_TE_KORT = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: Houdbaarheid van %4$sBuisID te kort";

	public static final String COLON_HOUDBAARHEID_ONBEKEND = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: Houdbaarheid van %4$sBuisID kan niet bepaald worden";

	public static final String COLON_ONBEKENDE_FOUT = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: Onbekende fout (%4$s)";

	public static final String COLON_BUISID_MIST_BIJ_TYPE_UITNODIGING = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: %4$s BuisID mist bij type uitnodiging";

	public static final String COLON_BUISID_ONVERWACHT_TYPE_UITNODIGING = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: %4$s BuisID onverwacht bij type uitnodiging";

	public static final String COLON_BUISID_IS_DUBBEL = "UitnodigingsID %1$s, BuisID %2$s, Extra BuisID %3$s: De BuisID is twee keer teruggekoppeld";

	public static final String CERVIX_UITNODIGINGSID_ONBEKEND = "UitnodigingsID %1$s, ZasID %2$s: UitnodigingsID is niet bekend";

	public static final String CERVIX_ZASID_AL_GEKOPPELD = "UitnodigingsID %1$s, ZasID %2$s: ZasID is al gekoppeld aan ander UitnodigingsId %3$s";

	public static final String CERVIX_UITNODIGINGSID_AL_GEKOPPELD = "UitnodigingsID %1$s, ZasID %2$s: UitnodigingsId is al gekoppeld aan ander ZasID %3$s";

	public static final String CERVIX_ONBEKENDE_FOUT = "UitnodigingsID %1$s, ZasID %2$s: Onbekende fout (%3$s)";

	public static final String CERVIX_ZASID_MIST_BIJ_TYPE_UITNODIGING = "UitnodigingsID %1$s, ZasID %2$s: ZasID mist bij type uitnodiging";

	public static final String CERVIX_HOUDBAARHEID_ONBEKEND = "UitnodigingsID %1$s, ZasID %2$s: Houdbaarheid van ZasID kan niet bepaald worden";

	public static final String CERVIX_HOUDBAARHEID_TE_KORT = "UitnodigingsID %1$s, ZasID %2$s: Houdbaarheid van ZasID te kort";

	public static final String CERVIX_ZASID_IS_DUBBEL = "UitnodigingsID %1$s, ZasID %2$s: De ZasID is twee keer teruggekoppeld";

	public static final String COLON_KOPPEL_BARCODE_GOLD = "_BARCODE_IFOBT";

	public static final String COLON_KOPPEL_BARCODE_EXTRA = "_BARCODE_EXTRA";

	public static final String CERVIX_KOPPEL_BARCODE_ZAS = "_BARCODE_ZAS";

	public static final String CERVIX_KOPPEL_TYPE = "_TYPE";

	public static final String KOPPEL_BARCODE = "_BARCODE";

	public static final String KOPPEL_DATUM_VERZENDING = "_DATUM_VERZENDING";

	public static final String KOPPEL_UITNODIGING_ID = "_ID";

	public static final String DEFAULT_STRING = "<geen>";

	private KoppelConstants()
	{
	}

}
