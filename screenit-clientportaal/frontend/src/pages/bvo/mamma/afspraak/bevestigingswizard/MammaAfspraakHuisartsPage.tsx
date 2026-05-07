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
import MammaAfspraakBevestigingsWizard from "./MammaAfspraakBevestigingsWizard"
import classNames from "classnames"
import styles from "./MammaAfspraakBevestigingsWizard.module.scss"
import SubmitButton from "../../../../../components/input/SubmitButton"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import properties from "./MammaAfspraakBevestigingsWizard.json"
import {useSelector} from "react-redux"
import {State} from "../../../../../datatypes/State"
import {Huisarts, MammaGeenHuisartsOptie} from "../../../../../datatypes/Huisarts"
import {bevestigVorige} from "../../../../../api/HuisartsThunkAction"
import {showToast} from "../../../../../utils/ToastUtil"
import {getContactUrl} from "../../../../../utils/UrlUtil"
import {useNavigate} from "react-router"
import huisartsProperties from "../../../gedeeld/huisarts/HuisartsPage.json"
import AfsluitenLink from "../../../../../components/afsluiten_link/AfsluitenLink"
import {FC, useEffect, useRef, useState} from "react"
import Button from "../../../../../components/input/Button"
import HuisartsSelectieComponent from "../../../../../components/huisarts_selectie/HuisartsSelectieComponent"
import {getString} from "../../../../../utils/TekstPropertyUtil"
import {Bevolkingsonderzoek} from "../../../../../datatypes/Bevolkingsonderzoek"
import {useSelectedBvo} from "../../../../../utils/Hooks"
import SpanWithHtml from "../../../../../components/span/SpanWithHtml"
import huisartsPageStyles from "../../../gedeeld/huisarts/HuisartsPage.module.scss"
import HuisartsView from "../../../../../components/huisarts_view/HuisartsView"
import {useWizardStap} from "../../../../../components/wizard_indicator/WizardIndicatorContext"
import {OpenstaandeOnderzoekenPopup} from "../openstaande-onderzoeken/OpenstaandeOnderzoekenPopup"
import {getOpenstaandeUitnodigingen} from "../../../../../api/OpenstaandeUitnodigingenThunkAction"
import {useThunkDispatch} from "../../../../../index"

const MammaAfspraakHuisartsPage: FC = () => {
	const bvo = useSelectedBvo()
	const dispatch = useThunkDispatch()
	const huidigeHuisarts = useSelector((state: State) => state.client.mammaDossier.huisartsHuidigeRonde)
	const vorigeHuisarts = useSelector((state: State) => state.client.mammaDossier.huisartsVorigeRonde)
	const openstaandeOnderzoeken = useSelector((state: State) => state.client.openstaandeUitnodigingen)
	const mammaHuidigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.geenHuisartsOptieHuidigeRonde : undefined)
	const mammaVorigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.geenHuisartsOptieVorigeRonde : undefined)
	const magHuisartsOntkoppelen = true
	const navigate = useNavigate()
	const huidigeStap = useWizardStap()
	const [toonOpenstaandePopup, setToonOpenstaandePopup] = useState(false)

	const initieleHuisarts = useRef(huidigeHuisarts)
	const [gekozenHuisarts, setGekozenHuisarts] = useState<Huisarts | undefined>(undefined)
	const [toonVerwijderPopup, setToonVerwijderPopup] = useState(false)
	const [toonWijzigen, setToonWijzigen] = useState(false)

	function heeftOpenstaandeOnderzoeken(): boolean {
		return !!openstaandeOnderzoeken && openstaandeOnderzoeken.length > 0
	}

	function bevestigHuisarts(): void {
		if (gekozenHuisarts) {
			opslaanEnAfronden()
			return
		}

		if (vorigeHuisarts) {
			dispatch(
				bevestigVorige(vorigeHuisarts, mammaVorigeGeenHuisartsOptie, Bevolkingsonderzoek.MAMMA),
			).then(() => opslaanEnAfronden())
			return
		}

		opslaanEnAfronden()
	}

	function opslaanEnAfronden(): void {
		showToast(
			huisartsProperties.gedeeld.toasts.opgeslagen.title,
			huisartsProperties.gedeeld.toasts.opgeslagen.description,
		)
		if (heeftOpenstaandeOnderzoeken()) {
			setToonOpenstaandePopup(true)
		} else {
			navigeerNaarHome()
		}
	}

	function navigeerNaarHome(): void {
		navigate("/mamma")
	}

	useEffect(() => {
		dispatch(getOpenstaandeUitnodigingen())
	}, [])

	function getGeenHuisartsOptieTekst(optie?: MammaGeenHuisartsOptie): string {
		switch (optie) {
			case MammaGeenHuisartsOptie.HUISARTS_IN_HET_BUITENLAND:
				return properties.HUISARTS_IN_HET_BUITENLAND
			case MammaGeenHuisartsOptie.TEHUIS_HUISARTS:
				return properties.TEHUIS_HUISARTS
			case MammaGeenHuisartsOptie.CLIENT_WIL_HUISARTS_NIET_OPGEVEN:
				return properties.CLIENT_WIL_HUISARTS_NIET_OPGEVEN
			case MammaGeenHuisartsOptie.HUISARTS_STAAT_ER_NIET_TUSSEN:
				return properties.HUISARTS_STAAT_ER_NIET_TUSSEN
			default:
				return ""
		}
	}

	return (
		toonOpenstaandePopup ? (
			<OpenstaandeOnderzoekenPopup
				openstaandeOnderzoeken={openstaandeOnderzoeken}
			/>
		) : (
			<div>
			<div>
				{huidigeHuisarts && !toonWijzigen ? (
					<div>
						<p>
							<SpanWithHtml
								className={huisartsPageStyles.infoText}
								value={properties.huisarts.description}/>
						</p>
						<p>{properties.huisarts.doorgeven.bekend.description}</p>
						<HuisartsView huisarts={huidigeHuisarts} andereHuisartsKiezen={() => setToonWijzigen(true)}/>
						<SpanWithHtml value={properties.huisarts.doorgeven.bekend.bevestiging_vraag}/>
					</div>
				) : (
					<HuisartsSelectieComponent
						description={
							huidigeHuisarts ? getString(properties.mamma.zoekpagina.description_wijzigen) : getString(properties.mamma.zoekpagina.description_toevoegen)
						}
						huidigeHuisarts={huidigeHuisarts}
						geenHuisartsTekst={getGeenHuisartsOptieTekst()}
						magOntkoppelen={magHuisartsOntkoppelen}
						mammaHuidigeGeenHuisartsOptie={mammaHuidigeGeenHuisartsOptie}
						analyticsCategorie={AnalyticsCategorie.MAMMA_AFSPRAAK}
						onHuisartsGekozen={h => {
							setGekozenHuisarts(h)
						}}
						onHuisartsVerwijderen={() => {
							setToonVerwijderPopup(true)
						}}
						onAnnulerenVerwijderen={() => {
							datadogService.stuurEvent("HuisartsVerwijderenAnnuleren", AnalyticsCategorie.MAMMA)
							setToonVerwijderPopup(false)
						}}
						onBevestigenVerwijderen={() => {
							datadogService.stuurEvent("HuisartsVerwijderd", AnalyticsCategorie.MAMMA)
							setToonVerwijderPopup(false)
							showToast(huisartsProperties.gedeeld.toasts.geen.title, huisartsProperties.gedeeld.toasts.geen.description)
						}}
						contactUrl={getContactUrl()}
						toonBlob={true}
					/>
				)}
			</div>
			<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
				<Button lightStyle={true} displayArrow={ArrowType.ARROW_LEFT} onClick={() => navigate("/mamma/afspraak/overzicht/")}
						label={properties.afspraak_maken.button.vorige}/>
				<div className={styles.knoppenRechts}>
					<SubmitButton displayArrow={ArrowType.ARROW_RIGHT}
								  label={properties.afspraak_maken.button.afronden}
								  onClick={() => {
									  const huisartsBekend = initieleHuisarts.current ? "huisarts bekend" : "huisarts niet bekend"
									  datadogService.stuurEvent("huisartsToegevoegd", AnalyticsCategorie.MAMMA_AFSPRAAK, {
										  naam: huisartsBekend,
										  stap: huidigeStap,
									  })
									  bevestigHuisarts()
								  }}/>
					<AfsluitenLink/>
				</div>
			</div>
		</div>
		)
	)
}

const WrappedMammaAfspraakHuisartsPage: FC = () => (
	<MammaAfspraakBevestigingsWizard>
		<MammaAfspraakHuisartsPage/>
	</MammaAfspraakBevestigingsWizard>
)

export default WrappedMammaAfspraakHuisartsPage
