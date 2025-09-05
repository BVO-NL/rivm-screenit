/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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
import {connect} from "react-redux"
import OnderzoekView, {OnderzoekViewProps} from "./OnderzoekView"
import type {Afspraak} from "../../datatypes/Afspraak"
import type {Client} from "../../datatypes/Client"
import type {ClientWerklijstItem} from "../../datatypes/ClientWerklijstItem"
import {getMandatory} from "../../util/MapUtil"
import type {Amputatie, Onderzoek} from "../../datatypes/Onderzoek"
import {RootState, store} from "../../Store"
import {showErrorToast} from "../../util/ToastUtil"
import type {AnnotatieAfbeelding} from "../../datatypes/AnnotatieAfbeelding"
import {MAMMOGRAFIE, OnderzoekType} from "../../datatypes/OnderzoekType"

const mapStateToProps = (state: RootState): OnderzoekViewProps => {
	const client = getMandatory(state.clientenById, state.navigation.clientId)
	const afspraak = getMandatory(state.afsprakenById, state.navigation.afspraakId)
	const studyToShowOnIms = afspraak.uitnodigingsNr
	return {
		client: client,
		afspraak: afspraak,
		subPagina: state.navigation.subPagina,
		gebruikersnaam: state.session?.gebruikersnaam,
		studyForIms: studyToShowOnIms,
		activeStudyForIms: state.activeStudyForIms !== null ? state.activeStudyForIms : undefined,
	}
}

export const maakWerklijstItem = (afspraak: Afspraak, client: Client, aeTitle: string, medewerkercode: string, onderzoekType: OnderzoekType): ClientWerklijstItem => {
	return {
		bsn: client.bsn,
		uitnodigingsNr: afspraak.uitnodigingsNr,
		geboortedatum: client.geboortedatum,
		voorletters: client.voorletters,
		tussenvoegsel: client.geboorteTussenvoegsel,
		achternaam: client.geboorteAchternaam,
		startDatumTijd: `${afspraak.vanafDatum}T${afspraak.vanafTijd}`,
		geslacht: client.geslacht,
		aeTitle,
		medewerkercode: medewerkercode,
		onderzoekscode: MAMMOGRAFIE === onderzoekType ? "SCREEN" : "SCREENDBT",
	}
}

export const verbodenWegensAmputatie = (afspraakId: number, amputatie: Amputatie): boolean => {
	const onderzoek: Onderzoek = getMandatory(store.getState().onderzoekByAfspraakId, afspraakId)
	const result: boolean = onderzoek.amputatie === amputatie

	if (result) {
		showErrorToast(`De ${amputatie.toLowerCase()} is gemarkeerd als geamputeerd, daarom is het niet mogelijk er een ander icoon op te plaatsen.`)
	}

	return result
}

export const verbodenWegensSignaleringsicoon = (afspraakId: number, amputatie: Amputatie): boolean => {
	const signalering = store.getState().signaleringByAfspraakId.get(afspraakId)
	if (!signalering) {
		return false
	}

	const afbeeldingen = signalering.doorsnedeAfbeeldingen
	const result = (amputatie === "RECHTERBORST" && (heeftIcoon(afbeeldingen.rechtsVerticaleDoorsnede) || heeftIcoon(afbeeldingen.rechtsHorizontaleDoorsnede)))
		|| (amputatie === "LINKERBORST" && (heeftIcoon(afbeeldingen.linksVerticaleDoorsnede) || heeftIcoon(afbeeldingen.linksHorizontaleDoorsnede)))

	if (result) {
		showErrorToast("Het is niet mogelijk om deze borst als geamputeerd te markeren. Bij het signaleren zijn reeds annotaties geplaatst, verwijder deze indien nodig.")
	}

	return result
}

const heeftIcoon = (doorsnede: AnnotatieAfbeelding | undefined): boolean => {
	return !!(doorsnede && doorsnede.iconenById && doorsnede.iconenById.size > 0)
}

const OnderzoekContainer = connect(mapStateToProps)(OnderzoekView)
export default OnderzoekContainer
