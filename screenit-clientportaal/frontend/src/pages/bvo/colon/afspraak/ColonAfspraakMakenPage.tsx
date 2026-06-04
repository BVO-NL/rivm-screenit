/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import {useEffect, useState} from "react"
import {BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import BasePage from "../../../BasePage"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {getHuidigeIntakeAfspraak} from "../../../../api/ColonIntakeAfspraakThunkAction"
import {useSelector} from "react-redux"
import {useThunkDispatch} from "../../../../index"
import properties from "./ColonAfspraakMakenPage.json"
import {splitAdresString} from "../../../../utils/StringUtil"
import {useLocation} from "react-router"
import {selectIntakeAfspraak} from "../../../../selectors/ColonSelectors"
import {VrijSlotZonderKamer} from "../../../../datatypes/VrijSlotZonderKamer"
import ColonAfspraakZoekenComponent from "../../../../components/colon_afspraak_zoeken/ColonAfspraakZoekenComponent"
import ColonAfspraakMakenBevestigingsPopup from "./ColonAfspraakMakenBevestigingsPopup"
import {getVolledigeAdresString} from "../../../../utils/AdresUtil"
import {weergaveColonIntake} from "../../../../utils/DateUtil"
import {ColonIntakeafspraakType} from "../../../../datatypes/colon/ColonIntakeafspraakType"

const ColonAfspraakMakenPage = () => {
	const [isNieuweAfspraak, setIsNieuweAfspraak] = useState<boolean>(false)
	const dispatch = useThunkDispatch()
	const huidigeIntakeAfspraak = useSelector(selectIntakeAfspraak)
	const [gekozenAfspraak, setGekozenAfspraak] = useState<VrijSlotZonderKamer | undefined>(undefined)
	const afspraakNaHeraanmelding = useLocation().pathname.includes("heraanmelding")

	useEffect(() => {
		dispatch(getHuidigeIntakeAfspraak())
	}, [dispatch])

	useEffect(() => {
		if (huidigeIntakeAfspraak && (huidigeIntakeAfspraak.afspraakAfgezegd || huidigeIntakeAfspraak.andereIntakelocatieOpVerzoekClient)) {
			setIsNieuweAfspraak(huidigeIntakeAfspraak.afspraakAfgezegd || huidigeIntakeAfspraak.andereIntakelocatieOpVerzoekClient)
		}
	}, [huidigeIntakeAfspraak])

	return (
		<BasePage bvoName={BevolkingsonderzoekNaam.COLON}
		          toonBlob={!isNieuweAfspraak}
		          title={getString(!isNieuweAfspraak ? properties.page.title.verzetten : properties.page.title.maken)}
		          description={getString(!isNieuweAfspraak ? properties.page.description.verzetten :
					  afspraakNaHeraanmelding ? properties.page.description.heraangemeld + properties.page.description.maken :
						  properties.page.description.maken)}
		          blobTitle={!isNieuweAfspraak ? getString(properties.blob.title) : ""}
		          blobText={!isNieuweAfspraak && huidigeIntakeAfspraak ? getString(properties.blob.afspraak.moment, [weergaveColonIntake(huidigeIntakeAfspraak.vanaf)]) : ""}
		          blobAdresLocatie={!isNieuweAfspraak && huidigeIntakeAfspraak ? getString(properties.blob.afspraak.locatie, [huidigeIntakeAfspraak.naamIntakelocatie, splitAdresString(getVolledigeAdresString(huidigeIntakeAfspraak.adres))]) : ""}>

			<ColonAfspraakZoekenComponent
				onAfspraakGeselecteerd={afspraak => setGekozenAfspraak({...afspraak, type: ColonIntakeafspraakType.OP_LOCATIE})}></ColonAfspraakZoekenComponent>

			{gekozenAfspraak &&
				<ColonAfspraakMakenBevestigingsPopup afspraak={gekozenAfspraak}
				                                     heraanmelding={isNieuweAfspraak}
				                                     onClose={() => setGekozenAfspraak(undefined)}/>
			}
		</BasePage>
	)
}

export default ColonAfspraakMakenPage
