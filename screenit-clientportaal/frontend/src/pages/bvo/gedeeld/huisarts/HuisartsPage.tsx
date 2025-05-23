/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal-frontend
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
import React, {useCallback, useEffect, useState} from "react"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../../datatypes/Bevolkingsonderzoek"
import BasePage from "../../../BasePage"
import styles from "./HuisartsPage.module.scss"
import type {Huisarts, HuisartsZoekobject} from "../../../../datatypes/Huisarts"
import {geenHuisartsZoekresultaten, leegHuisartsZoekobject, MammaGeenHuisartsOptie} from "../../../../datatypes/Huisarts"
import {Col, Row} from "react-bootstrap"
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
import ScreenitBackend from "../../../../utils/Backend"
import {useThunkDispatch} from "../../../../index"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {createShowToastAction} from "../../../../actions/ToastAction"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {useRegio, useSelectedBvo} from "../../../../utils/Hooks"
import {getBvoBaseUrl, getContactUrl} from "../../../../utils/UrlUtil"
import ActieBasePage from "../../../ActieBasePage"
import {Formik} from "formik"
import * as Yup from "yup"
import bvoStyle from "../../../../components/BvoStyle.module.scss"
import HuisartsHintComponent from "./HuisartsHintComponent"
import Button from "../../../../components/input/Button"
import {concatWithSpace} from "../../../../utils/StringUtil"
import {HuisartsBevestigingsPopup, HuisartsBevestigingsPopupType} from "./HuisartsBevestigingsPopup"
import {REGEX_POSTCODE_SEARCH} from "../../../../validators/AdresValidator"
import ScreenitTextfield from "../../../../components/input/ScreenitTextfield"
import {FormErrorComponent} from "../../../../components/form_error/FormErrorComponent"
import BigUrlButton from "../../../../components/bigUrlButton/BigUrlButton"
import AdvancedSearchLinkComponent from "../../../../components/form/AdvancedSearchLinkComponent"
import SearchResultHuisarts from "../../../../components/search_results/SearchResultHuisarts"
import SubmitForm from "../../../../components/form/SubmitForm"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import SearchForm from "../../../../components/form/SearchForm"
import {useLocation, useNavigate} from "react-router"
import {showToast} from "../../../../utils/ToastUtil"

const HuisartsPage = () => {
	const dispatch = useThunkDispatch()
	const bvo = useSelectedBvo()
	const regio = useRegio()
	const properties = require("./HuisartsPage.json")
	const navigate = useNavigate()
	const location = useLocation()

	const [gekozenHuisarts, setGekozenHuisarts] = useState<Huisarts | undefined>(undefined)
	const [wiltHuisartsVerwijderen, setWiltHuisartsVerwijderen] = useState<boolean>(false)

	const [disableZoekMeerButton, setDisableZoekMeerButton] = useState<boolean>(true)
	const [zoekObject, setZoekObject] = useState<HuisartsZoekobject>(leegHuisartsZoekobject)
	const [zoekResultaten, setZoekResultaten] = useState<Huisarts[]>(geenHuisartsZoekresultaten)
	const [wilZoeken, setWilZoeken] = useState<boolean>(location.pathname.includes("zoeken"))
	const [gezocht, setGezocht] = useState<boolean>(false)
	const [isAdvancedSearch, setAdvancedSearch] = useState<boolean>(false)

	const huidigeHuisarts = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.huisartsHuidigeRonde : state.client.colonDossier.huisartsHuidigeRonde)
	const vorigeHuisarts = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.huisartsVorigeRonde : state.client.colonDossier.huisartsVorigeRonde)
	const magHuisartsOntkoppelen = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.magHuisartsOntkoppelen : true)
	const mammaHuidigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.geenHuisartsOptieHuidigeRonde : undefined)
	const mammaVorigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => bvo === Bevolkingsonderzoek.MAMMA ? state.client.mammaDossier.geenHuisartsOptieVorigeRonde : undefined)

	const geenResultaten = zoekResultaten.length === 0 && gezocht

	const bevolkingsonderzoekNaam = bvo === Bevolkingsonderzoek.MAMMA ? BevolkingsonderzoekNaam.MAMMA : BevolkingsonderzoekNaam.COLON

	enum HuisartsActieKeuze {
		BEVESTIGEN = "BEVESTIGEN",
		ZOEKEN = "ZOEKEN",
		ANNULEREN = "ANNULEREN",
	}

	const initialSearchFormValues = {
		naam: "",
		adres: {
			plaats: "",
			postcode: "",
			straat: "",
		},
	}

	const searchFormValidatieSchema: Yup.AnyObjectSchema = Yup.object({
		adres: Yup.object().shape({
			postcode: Yup.string().matches(REGEX_POSTCODE_SEARCH, properties.gedeeld.zoekpagina.zoekform.validation.postcode),
		}),
	})

	useEffect(() => {
		setDisableZoekMeerButton(zoekResultaten.length === 0 || zoekResultaten.length % 10 !== 0)
	}, [setDisableZoekMeerButton, zoekResultaten])

	useEffect(() => {
		dispatch(getHuidigeHuisarts(bvo))
		dispatch(getVorigeHuisarts(bvo))
		if (bvo === Bevolkingsonderzoek.MAMMA) {
			dispatch(magHuisartsKoppelen(bvo))
			dispatch(getHuidigeMammaGeenHuisartsOptie(bvo))
			dispatch(getVorigeMammaGeenHuisartsOptie(bvo))
		}
	}, [dispatch, bvo])

	const zoekHuisartsen = useCallback((zoekobject: HuisartsZoekobject, paginaNummer: number) => {
		return ScreenitBackend.post(`huisarts?paginaNummer=${paginaNummer}`, zoekobject)
			.then((response) => {
				paginaNummer > 0 ? setZoekResultaten(zoekResultaten.concat(response.data)) : setZoekResultaten(response.data)
				setGezocht(true)
			})
	}, [setZoekResultaten, zoekResultaten])

	const zoek = useCallback((zoekObject: HuisartsZoekobject) => {
		setZoekObject(zoekObject)
		setZoekResultaten(geenHuisartsZoekresultaten)
		zoekHuisartsen(zoekObject, 0)
		setDisableZoekMeerButton(false)
	}, [setZoekObject, setDisableZoekMeerButton, zoekHuisartsen, setZoekResultaten])

	const zoekMeer = useCallback(() => {
		const paginaNummer = zoekResultaten.length / 10
		zoekHuisartsen(zoekObject, paginaNummer)
	}, [zoekResultaten, zoekHuisartsen, zoekObject])

	function kiesHuisarts(huisarts: Huisarts) {
		if (huidigeHuisarts && huidigeHuisarts.id === huisarts.id) {
			dispatch(createShowToastAction({
				title: getString(properties.gedeeld.toasts.zelfde.title),
				description: getString(properties.gedeeld.toasts.zelfde.description),
				alGetoond: false,
			}))
		} else {
			setGekozenHuisarts(huisarts)
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
					<HuisartsHintComponent geenHuisartsOptie={mammaVorigeGeenHuisartsOptie ? properties[mammaVorigeGeenHuisartsOptie] : undefined}
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
				blobText={(huidigeHuisarts && concatWithSpace(huidigeHuisarts.voorletters, huidigeHuisarts.achternaam)) || (mammaHuidigeGeenHuisartsOptie && properties[mammaHuidigeGeenHuisartsOptie])}
				blobAdresLocatie={huidigeHuisarts && getString(properties.gedeeld.blob.locatie, huidigeHuisarts && [huidigeHuisarts.praktijknaam, concatWithSpace(huidigeHuisarts.adres.straat, huidigeHuisarts.adres.huisnummer), concatWithSpace(huidigeHuisarts.adres.postcode, huidigeHuisarts.adres.plaats)])}
				onBlobLinkClick={() => {
					if (!magHuisartsOntkoppelen) {
						showToast(getString(properties.gedeeld.toasts.geen.title), getString(properties.gedeeld.toasts.geen.description))
					} else if (huidigeHuisarts || mammaHuidigeGeenHuisartsOptie) {
						setWiltHuisartsVerwijderen(true)
					}
				}}
				blobLinkText={getString(properties.gedeeld.zoekpagina.blob.link)}>

				<Row>
					<Col md={5}>
						<Formik initialValues={initialSearchFormValues}
								validationSchema={searchFormValidatieSchema}
								onSubmit={zoek}>
							{({errors, values, initialValues, setFieldValue, handleSubmit}) => (
								<SearchForm title={getString(properties.gedeeld.zoekpagina.zoekform.title)}>

									<ScreenitTextfield onChange={value => setFieldValue("naam", value)}
													   value={values.naam}
													   name={"naam"}
													   placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.naam)}/>

									<ScreenitTextfield onChange={value => setFieldValue("adres.plaats", value)}
													   value={values.adres.plaats}
													   name={"adres.plaats"}
													   placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.plaats)}/>

									<div className={styles.advancedSearchButton}
										 onClick={async () => {
											 await setFieldValue("adres.postcode", "")
											 await setFieldValue("adres.straat", "")
											 setAdvancedSearch(!isAdvancedSearch)
										 }}>
										<AdvancedSearchLinkComponent advancedSearch={isAdvancedSearch}/>
									</div>
									{isAdvancedSearch && <div>
										<ScreenitTextfield onChange={value => setFieldValue("adres.postcode", value)}
														   value={values.adres.postcode}
														   name={"adres.postcode"}
														   placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.postcode)}
														   invalidMessage={errors.adres?.postcode}/>

										<ScreenitTextfield onChange={value => setFieldValue("adres.straat", value)}
														   value={values.adres.straat}
														   name={"adres.straat"}
														   placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.straat)}/>
									</div>}
									<Button className={bvoStyle.darkBackgroundColor}
											label={getString(properties.gedeeld.zoekpagina.zoekform.submit)}
											displayArrow={ArrowType.ARROW_RIGHT}
											onClick={handleSubmit}/>
								</SearchForm>)}
						</Formik>
					</Col>
					<Col md={7} className={styles.results}>

						{geenResultaten &&
							<FormErrorComponent text={getString(properties.gedeeld.zoekpagina.resultaten.geen)}/>}

						{geenResultaten &&
							<BigUrlButton title={getString(properties.gedeeld.zoekpagina.resultaten.button.header)}
										  text={getString(properties.gedeeld.zoekpagina.resultaten.button.text)}
										  link={getContactUrl(regio)}/>}

						{zoekResultaten.map((resultaat) =>
							<SearchResultHuisarts
								key={resultaat.id}
								className={styles.results}
								col1={["", resultaat.praktijknaam, concatWithSpace(resultaat.voorletters, resultaat.achternaam)]}
								col2={[getString(properties.gedeeld.zoekpagina.resultaten.resultaat.locatie_header), concatWithSpace(resultaat.adres.straat, resultaat.adres.huisnummer), concatWithSpace(resultaat.adres.postcode, resultaat.adres.plaats)]}
								onHoverText={getString(properties.gedeeld.zoekpagina.resultaten.resultaat.hover)}
								onClickAction={() => kiesHuisarts(resultaat)}
							/>,
						)}
						{gezocht && !disableZoekMeerButton && !geenResultaten && <div className={styles.showMoreResultsButtonArea}>
							<Button
								label={getString(properties.gedeeld.zoekpagina.resultaten.meer)}
								displayArrow={ArrowType.ARROW_DOWN}
								arrowBeforeLabel={false}
								onClick={zoekMeer}/>
						</div>}
					</Col>
				</Row>
				{
					(gekozenHuisarts &&
						<HuisartsBevestigingsPopup
							huisarts={gekozenHuisarts}
							type={HuisartsBevestigingsPopupType.BEVESTIGEN}
							onPrimaireKnop={() => {
								showToast(getString(properties.gedeeld.toasts.opgeslagen.title), getString(properties.gedeeld.toasts.opgeslagen.description))
								navigate(getBvoBaseUrl(bvo))
							}}
							onSecundaireKnop={() => setGekozenHuisarts(undefined)}
						/>)
					|| (wiltHuisartsVerwijderen && (huidigeHuisarts || mammaHuidigeGeenHuisartsOptie) &&
						<HuisartsBevestigingsPopup
							huisarts={huidigeHuisarts}
							geenHuisartsOpie={mammaHuidigeGeenHuisartsOptie ? properties[mammaHuidigeGeenHuisartsOptie] : undefined}
							type={HuisartsBevestigingsPopupType.VERWIJDEREN}
							onPrimaireKnop={() => {
								showToast(getString(properties.gedeeld.toasts.geen.title), getString(properties.gedeeld.toasts.geen.description))
								navigate(getBvoBaseUrl(bvo))
							}}
							onSecundaireKnop={() => setWiltHuisartsVerwijderen(false)}
						/>)
				}
			</BasePage>
		)
	}

	return toonKeuzePagina()
}

export default HuisartsPage
