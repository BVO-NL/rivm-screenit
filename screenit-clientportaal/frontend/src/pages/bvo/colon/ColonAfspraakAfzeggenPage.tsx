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
import {useThunkDispatch} from "../../../index"
import {FC, useEffect} from "react"
import ActieBasePage from "../../ActieBasePage"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import {getString} from "../../../utils/TekstPropertyUtil"
import {getHuidigeIntakeAfspraak} from "../../../api/ColonIntakeAfspraakThunkAction"
import {useSelector} from "react-redux"
import properties from "./ColonAfspraakAfzeggenPage.json"
import LadenComponent from "../../../components/laden/LadenComponent"
import {splitAdresString} from "../../../utils/StringUtil"
import ColonAfspraakAfzeggenForm from "../../../components/form/colon/ColonAfspraakAfzeggenForm"
import {getVolledigeAdresString} from "../../../utils/AdresUtil"
import {selectIntakeAfspraak} from "../../../selectors/ColonSelectors"
import {ColonIntakeafspraakType} from "../../../datatypes/colon/ColonIntakeafspraakType"
import {weergaveColonDigitaleIntakeWeek, weergaveColonIntake} from "../../../utils/DateUtil"

const ColonAfspraakAfzeggenPage: FC = () => {
	const dispatch = useThunkDispatch()
	const huidigeIntakeAfspraak = useSelector(selectIntakeAfspraak)
	useEffect(() => {
		dispatch(getHuidigeIntakeAfspraak())
	}, [dispatch])

	if (!huidigeIntakeAfspraak) {
		return <LadenComponent/>
	}

	const datumWeergave = huidigeIntakeAfspraak.type === ColonIntakeafspraakType.OP_LOCATIE ? `${weergaveColonIntake(huidigeIntakeAfspraak.vanaf)} uur` : weergaveColonDigitaleIntakeWeek(huidigeIntakeAfspraak.vanaf)

	return (
		<ActieBasePage bvoName={BevolkingsonderzoekNaam[Bevolkingsonderzoek.COLON]}
		               title={getString(properties.page.title)}
		               description={getString(properties.page.description)}
		               hintBegin={getString(properties.huidige_afspraak, [datumWeergave, huidigeIntakeAfspraak.naamIntakelocatie, splitAdresString(getVolledigeAdresString(huidigeIntakeAfspraak.adres))])}>

			<ColonAfspraakAfzeggenForm/>

		</ActieBasePage>
	)

}

export default ColonAfspraakAfzeggenPage
