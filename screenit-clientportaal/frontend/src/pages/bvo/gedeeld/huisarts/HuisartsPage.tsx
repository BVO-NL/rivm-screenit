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
import * as React from "react"
import {useEffect, useState} from "react"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import BasePage from "../../../BasePage"
import styles from "./HuisartsPage.module.scss"
import {Huisarts, MammaGeenHuisartsOptie} from "../../../../datatypes/Huisarts"
import {useSelector} from "react-redux"
import {State} from "../../../../datatypes/State"
import {
	bevestigVorige,
	getHuidigeHuisarts,
	getHuidigeMammaGeenHuisartsOptie,
	getVorigeHuisarts,
	getVorigeMammaGeenHuisartsOptie,
	magHuisartsKoppelen,
} from "../../../../api/HuisartsThunkAction"
import {useThunkDispatch} from "../../../../index"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {useSelectedBvo} from "../../../../utils/Hooks"
import {getBvoBaseUrl, getContactUrl} from "../../../../utils/UrlUtil"
import ActieBasePage from "../../../ActieBasePage"
import {Formik} from "formik"
import * as Yup from "yup"
import HuisartsHintComponent from "./HuisartsHintComponent"
import {HuisartsBevestigingsPopup, HuisartsBevestigingsPopupType} from "./HuisartsBevestigingsPopup"
import SubmitForm from "../../../../components/form/SubmitForm"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import {useLocation, useNavigate} from "react-router"
import {showToast} from "../../../../utils/ToastUtil"
import properties from "./HuisartsPage.json"
import datadogService from "../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"
import HuisartsSelectieComponent from "../../../../components/huisarts_selectie/HuisartsSelectieComponent"
import {concatWithSpace} from "../../../../utils/StringUtil"

const HuisartsPage = () => {
	const dispatch = useThunkDispatch()
	const bvo = useSelectedBvo()
	const navigate = useNavigate()
	const location = useLocation()

	const [gekozenHuisarts, setGekozenHuisarts] = useState<Huisarts | undefined>(undefined)
	const [wiltHuisartsVerwijderen, setWiltHuisartsVerwijderen] = useState<boolean>(false)

	const [wilZoeken, setWilZoeken] = useState<boolean>(location.pathname.includes("zoeken"))

	const huidigeHuisarts = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.huisartsHuidigeRonde : state.client.colonDossier.huisartsHuidigeRonde)
	const vorigeHuisarts = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.huisartsVorigeRonde : state.client.colonDossier.huisartsVorigeRonde)
	const magHuisartsOntkoppelen = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.magHuisartsOntkoppelen : true)
	const mammaHuidigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.geenHuisartsOptieHuidigeRonde : undefined)
	const mammaVorigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.geenHuisartsOptieVorigeRonde : undefined)

	const bevolkingsonderzoekNaam = bvo === Bevolkingsonderzoek.MAMMA ? BevolkingsonderzoekNaam.MAMMA : BevolkingsonderzoekNaam.COLON

	enum HuisartsActieKeuze {
		BEVESTIGEN = "BEVESTIGEN",
		ZOEKEN = "ZOEKEN",
		ANNULEREN = "ANNULEREN",
	}

	useEffect(() => {
		dispatch(getHuidigeHuisarts(bvo))
		dispatch(getVorigeHuisarts(bvo))
		if (bvo === Bevolkingsonderzoek.MAMMA) {
			dispatch(magHuisartsKoppelen(bvo))
			dispatch(getHuidigeMammaGeenHuisartsOptie(bvo))
			dispatch(getVorigeMammaGeenHuisartsOptie(bvo))
		}
	}, [dispatch, bvo])

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

	function bevestigHuisartsActieKeuze(keuze: HuisartsActieKeuze) {
		switch (keuze) {
			case HuisartsActieKeuze.BEVESTIGEN: {
				dispatch(bevestigVorige(vorigeHuisarts, mammaVorigeGeenHuisartsOptie, bvo)).then(() => {
					showToast(getString(properties.gedeeld.toasts.opgeslagen.title), getString(properties.gedeeld.toasts.opgeslagen.description))
					navigate(getBvoBaseUrl(bvo))
				})
				break
			}
			case HuisartsActieKeuze.ZOEKEN: {
				setWilZoeken(true)
				break
			}
			case HuisartsActieKeuze.ANNULEREN: {
				navigate(getBvoBaseUrl(bvo))
				break
			}
		}
	}

	const initialKeuzeFormValues = {
		keuze: undefined,
	}
	const keuzeFormValidatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		keuze: Yup.string().required(getString(properties.gedeeld.tussenpagina.form.error)),
	})

	function toonKeuzePagina() {
		if (!wilZoeken && !huidigeHuisarts && !mammaHuidigeGeenHuisartsOptie && (vorigeHuisarts || mammaVorigeGeenHuisartsOptie)) {
			return (
				<ActieBasePage className={styles.keuzePagina}
							   bvoName={bevolkingsonderzoekNaam}
							   title={getString(properties.gedeeld.title)}
							   description={
								   bvo === Bevolkingsonderzoek.MAMMA ? getString(properties.mamma.tussenpagina.description)
									   : getString(properties.colon.tussenpagina.description)
							   }>
					<HuisartsHintComponent geenHuisartsOptie={mammaVorigeGeenHuisartsOptie ? getGeenHuisartsOptieTekst(mammaVorigeGeenHuisartsOptie) : undefined}
										   huisarts={vorigeHuisarts}
										   isBold={true}/>

					<Formik initialValues={initialKeuzeFormValues}
							validationSchema={keuzeFormValidatieSchema}
							onSubmit={(values) => {
								bevestigHuisartsActieKeuze(values.keuze!)
							}}>
						{formikProps => (<SubmitForm title={properties.gedeeld.tussenpagina.form.title}
													 formikProps={formikProps}
													 buttonLabel={getString(properties.gedeeld.tussenpagina.form.submit)}>
							<FormControl variant="standard" required component="fieldset">
								<p className={styles.errorLabel}>{formikProps.errors.keuze}</p>
								<RadioGroup
									name="keuze"
									onChange={formikProps.handleChange}
									value={formikProps.values.keuze}>
									<ul>
										<li className={styles.radioGroup}>
											<FormControlLabel
												value={HuisartsActieKeuze.BEVESTIGEN}
												control={<Radio/>}
												label={getString(properties.gedeeld.tussenpagina.form.radiobuttons.bevestigen)}/>
											<FormControlLabel
												value={HuisartsActieKeuze.ZOEKEN}
												control={<Radio/>}
												label={getString(properties.gedeeld.tussenpagina.form.radiobuttons.zoeken)}/>
											<FormControlLabel
												value={HuisartsActieKeuze.ANNULEREN}
												control={<Radio/>}
												label={getString(properties.gedeeld.tussenpagina.form.radiobuttons.annuleren)}/>
										</li>
									</ul>
								</RadioGroup>
							</FormControl>
						</SubmitForm>)}
					</Formik>
				</ActieBasePage>
			)
		} else {
			return toonZoekpagina()
		}
	}

	function toonZoekpagina() {
		return (
			<BasePage
				bvoName={bevolkingsonderzoekNaam}
				title={getString(properties.gedeeld.title)}
				description={
					bvo === Bevolkingsonderzoek.MAMMA ? (huidigeHuisarts ? getString(properties.mamma.zoekpagina.description_wijzigen) : getString(properties.mamma.zoekpagina.description_toevoegen))
						: (huidigeHuisarts ? getString(properties.colon.zoekpagina.description_wijzigen) : getString(properties.colon.zoekpagina.description_toevoegen))
				}
				toonBlob={!!huidigeHuisarts || !!mammaHuidigeGeenHuisartsOptie}
				blobTitle={getString(properties.gedeeld.blob.title)}
				blobText={(huidigeHuisarts && concatWithSpace(huidigeHuisarts.voorletters, huidigeHuisarts.achternaam)) || getGeenHuisartsOptieTekst(mammaHuidigeGeenHuisartsOptie)}
				blobAdresLocatie={huidigeHuisarts && getString(properties.gedeeld.blob.locatie, huidigeHuisarts && [huidigeHuisarts.praktijknaam, concatWithSpace(huidigeHuisarts.adres.straat, huidigeHuisarts.adres.huisnummer), concatWithSpace(huidigeHuisarts.adres.postcode, huidigeHuisarts.adres.plaats)])}
				onBlobLinkClick={() => {
					if (!magHuisartsOntkoppelen) {
						showToast(getString(properties.gedeeld.toasts.geen.title), getString(properties.gedeeld.toasts.geen.description))
					} else if (huidigeHuisarts || mammaHuidigeGeenHuisartsOptie) {
						datadogService.stuurEvent("HuisartsVerwijderen", AnalyticsCategorie.MAMMA)
						setWiltHuisartsVerwijderen(true)
					}
				}}
				blobLinkText={getString(properties.gedeeld.zoekpagina.blob.link)}>

				<HuisartsSelectieComponent
					description=""
					huidigeHuisarts={huidigeHuisarts}
					mammaHuidigeGeenHuisartsOptie={mammaHuidigeGeenHuisartsOptie}
					magOntkoppelen={magHuisartsOntkoppelen}
					contactUrl={getContactUrl()}
					analyticsCategorie={AnalyticsCategorie.MAMMA_HUISARTS}

					onHuisartsGekozen={(huisarts) => {
						setGekozenHuisarts(huisarts)
						navigate(getBvoBaseUrl(bvo))
					}}

					onHuisartsVerwijderen={() => {
						setWiltHuisartsVerwijderen(true)
					}}

					onAnnulerenVerwijderen={() => {
						setWiltHuisartsVerwijderen(false)
					}}

					onBevestigenVerwijderen={() => {
						showToast(
							getString(properties.gedeeld.toasts.geen.title),
							getString(properties.gedeeld.toasts.geen.description),
						)

						navigate(getBvoBaseUrl(bvo))
					}}
					toonBlob={false}
				/>

				{gekozenHuisarts && (
					<HuisartsBevestigingsPopup
						huisarts={gekozenHuisarts}
						type={HuisartsBevestigingsPopupType.BEVESTIGEN}
						onPrimaireKnop={() => {
							showToast(
								getString(properties.gedeeld.toasts.opgeslagen.title),
								getString(properties.gedeeld.toasts.opgeslagen.description),
							)

							navigate(getBvoBaseUrl(bvo))
						}}
						onSecundaireKnop={() => setGekozenHuisarts(undefined)}
					/>
				)}

				{wiltHuisartsVerwijderen && huidigeHuisarts && (
					<HuisartsBevestigingsPopup
						huisarts={huidigeHuisarts}
						type={HuisartsBevestigingsPopupType.VERWIJDEREN}
						onPrimaireKnop={() => {
							showToast(
								getString(properties.gedeeld.toasts.geen.title),
								getString(properties.gedeeld.toasts.geen.description),
							)

							navigate(getBvoBaseUrl(bvo))
						}}
						onSecundaireKnop={() => setWiltHuisartsVerwijderen(false)}
					/>
				)}

			</BasePage>
		)
	}

	return toonKeuzePagina()
}

export default HuisartsPage
