package nl.rivm.screenit.service.colon;

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

import java.time.LocalDate;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonFitAnalyseResultaatSet;
import nl.rivm.screenit.model.colon.ColonFitRegistratie;
import nl.rivm.screenit.model.colon.ColonFitType;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.enums.ColonFitRegistratieStatus;

public interface ColonBaseFitService
{
	Optional<ColonFitRegistratie> getFit(String barcode);

	void verwerkAnalyseResultaat(ColonFitRegistratie registratie);

	void setStatus(ColonFitRegistratie registratie, ColonFitRegistratieStatus nieuweStatus);

	void heraanmelden(ColonScreeningRonde screeningRonde);

	void verwijderScannedAntwoordFormulier(ColonUitnodiging uitnodiging);

	void verwijderFitAnalyseResultaat(ColonFitRegistratie registratie, UploadDocument uploadDocument);

	void markeerRegistratieAlsVerloren(ColonUitnodiging uitnodiging);

	void monsterNietBeoordeelbaar(ColonFitRegistratie registratie);

	void checkVervaldatumVerlopen(ColonFitRegistratie registratie);

	void bepaalEnSetHeraanmeldenTekstKey(ColonFitRegistratie fitRegistratie);

	void setFitRegistratiesVerlorenIndienActief(ColonFitRegistratie registratie);

	Client getAndereClientOpZelfdeAdresEnActieveFit(Client client, List<Long> uitgenodigdeClientIds);

	Optional<ColonFitAnalyseResultaatSet> getFitAnalyseResultaatSet(String bestandsnaam);

	Optional<ColonFitRegistratie> getLaatsteFitMetMissendeUitslagVanDossier(ColonDossier dossier, LocalDate signalerenVanaf, LocalDate minimaleSignaleringsDatum);

	boolean isVerwijderdeBarcode(String barcode);

	String getToonbareWaarde(ColonFitRegistratie fit);

	boolean isDk2026Actief();

	void koppelTestIndienMogelijk(String fitBarcode, ColonFitType fitType, ColonUitnodiging uitnodiging, Date datumVerstuurd, ColonScreeningRonde screeningRonde);
}
