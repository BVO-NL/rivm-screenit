package nl.rivm.screenit.service;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import nl.rivm.screenit.dto.alg.client.contact.DeelnamewensDto;
import nl.rivm.screenit.dto.mamma.afspraken.IMammaAfspraakWijzigenFilter;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContact;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactActieType;
import nl.rivm.screenit.model.ClientContactActieTypeFilter;
import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.OrganisatieMedewerker;
import nl.rivm.screenit.model.RedenGbaVraag;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.TijdelijkAdres;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.ColonNieuwFitResultaat;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.mamma.MammaAfspraak;

public interface ClientContactService
{

	ClientContact getClientContactById(Long id);

	ClientContact maakClientContact(Client client, LocalDateTime moment, List<ClientContactActieType> acties, String opmerking,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	ClientContact maakClientContact(Client client, LocalDateTime moment, ClientContactActieType actie, String opmerking,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	ClientContact maakClientContact(Client client, ClientContactActieType actie, String opmerking, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	ClientContact updateClientContact(ClientContact contact, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	ClientContact verwijderNotitie(ClientContact contact, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void saveClientContact(ClientContact contact, Map<ClientContactActieType, Map<ExtraOpslaanKey, Object>> extraOpslaanObjecten, Account ingelogdeOrganisatieMedewerker);

	ClientContactActie mammaAfspraakMakenWijzigen(ClientContactActie actie, Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams, Account account,
		boolean isNieuweAfspraak, boolean isGeforceerdeAfspraak);

	void mammaAfspraakBevestigingMakenVanuitClientPortaal(Client client, Map<ExtraOpslaanKey, Object> extraOpslaanParams);

	void verstuurBevestigingsmail(MammaAfspraak afspraak, Account account, String emailAdres);

	List<ClientContactActieType> getAvailableActies(Client client);

	List<ClientContactActieType> getAvailableActies(Client client, boolean viaClientPortaal);

	List<ClientContactActieType> getAvailableActies(Client client, Collection<ClientContactActieType> teControlerenActieTypes);

	boolean availableActiesBevatBenodigdeActie(Client client, ClientContactActieType benodigdeActie);

	ColonNieuwFitResultaat vraagNieuweFitAan(Client client, Account account);

	boolean magNieuweFitAanvragen(Client client);

	void saveTijdelijkAdres(Account account, Client client, TijdelijkAdres tijdelijkAdres);

	void saveAanhef(Account account, Client client, Aanhef aanhef);

	void wijzigAanhef(Client client, Aanhef aanhef, String notitie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void registreerDeelnamewensen(Client client, DeelnamewensDto deelnamewensDto, String notitie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void vraagClientgegevensOpnieuwAan(Client client, RedenGbaVraag reden, String notitie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void vraagKeuzeGebruikGegevensBriefAan(Client client, BriefType briefType, boolean handtekeningVergeten, String notitie,
		OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void wijzigTijdelijkAdres(Client client, TijdelijkAdres tijdelijkAdres, String notitie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void heraanmeldenCervix(Client client, String notitie, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	<A extends Afmelding<?, ?, ?>> Optional<A> zoekAfmeldingOmHeraanTeMelden(Dossier<? extends ScreeningRonde<?, ?, ? extends A, ?>, ? extends A> dossier);

	List<AfmeldingType> getAvailableAfmeldoptiesColon(Client client, boolean viaPortaalGevraagd);

	List<AfmeldingType> getAvailableAfmeldoptiesMamma(Client object, boolean viaPortaalGevraagd);

	List<AfmeldingType> getAvailableAfmeldoptiesCervix(Client client, boolean viaPortaalGevraagd);

	boolean magNieuweUitnodigingAanvragen(Dossier dossier, boolean isHeraanmelding);

	boolean defaultNieuweUitnodigingAanvragen(Dossier dossier);

	boolean heeftOpenIntakeAfspraak(Client client);

	void updateContact(ClientContact contact, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	void verwijderContact(ClientContact contact, OrganisatieMedewerker ingelogdeOrganisatieMedewerker);

	boolean heeftOpenMammaAfspraak(Client client);

	boolean magNieuweIntakeAfspraakMakenNaHeraanmelding(ColonDossier colonDossier);

	boolean isAfspraakTijdBezet(MammaAfspraak nieuweAfspraak, IMammaAfspraakWijzigenFilter afspraakWijzigenFilter);

	List<ClientContact> getClientContacten(Client client, long first, long count, String sortProperty, boolean ascending);

	List<ClientContact> getClientContacten(Client client, ClientContactActieTypeFilter actieTypeFilter);

	Long countClientContactenMetOpmerking(Long clientId);

}
