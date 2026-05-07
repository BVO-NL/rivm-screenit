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
import React, {FC, useCallback, useEffect, useState} from "react"
import {Formik, FormikProps} from "formik"
import * as Yup from "yup"
import {Col, Row} from "react-bootstrap"

import SearchForm from "../form/SearchForm"
import Button from "../input/Button"
import ScreenitTextfield from "../input/ScreenitTextfield"
import SearchResultHuisarts from "../search_results/SearchResultHuisarts"
import BigUrlButton from "../bigUrlButton/BigUrlButton"
import {FormErrorComponent} from "../form_error/FormErrorComponent"

import {geenHuisartsZoekresultaten, Huisarts, HuisartsZoekobject, leegHuisartsZoekobject, MammaGeenHuisartsOptie} from "../../datatypes/Huisarts"
import {ArrowType} from "../vectors/ArrowIconComponent"
import {HuisartsBevestigingsPopup, HuisartsBevestigingsPopupType} from "../../pages/bvo/gedeeld/huisarts/HuisartsBevestigingsPopup"
import {concatWithSpace} from "../../utils/StringUtil"
import datadogService from "../../services/DatadogService"
import {AnalyticsCategorie} from "../../datatypes/AnalyticsCategorie"
import styles from "../../pages/bvo/gedeeld/huisarts/HuisartsPage.module.scss"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "../../pages/bvo/gedeeld/huisarts/HuisartsPage.json"
import AdvancedSearchLinkComponent from "../form/AdvancedSearchLinkComponent"
import {REGEX_POSTCODE_SEARCH} from "../../validators/AdresValidator"
import {showToast} from "../../utils/ToastUtil"
import TextBlobComponent from "../blob/TextBlobComponent"
import classNames from "classnames"
import SpanWithHtml from "../span/SpanWithHtml"
import {useThunkDispatch} from "../../index"
import {zoekHuisartsenAction} from "../../api/HuisartsThunkAction"
import bvoStyle from "../BvoStyle.module.scss"

export interface HuisartsSelectieComponentProps {
	description: string

	huidigeHuisarts?: Huisarts
	geenHuisartsTekst?: string
	mammaHuidigeGeenHuisartsOptie?: MammaGeenHuisartsOptie

	magOntkoppelen: boolean
	toonBlob: boolean

	analyticsCategorie: AnalyticsCategorie

	onHuisartsGekozen: (huisarts: Huisarts) => void
	onHuisartsVerwijderen: () => void
	onAnnulerenVerwijderen: () => void
	onBevestigenVerwijderen: () => void

	contactUrl: string
}

const searchSchema = Yup.object({
	adres: Yup.object().shape({
		postcode: Yup.string().matches(REGEX_POSTCODE_SEARCH, properties.gedeeld.zoekpagina.zoekform.validation.postcode),
	}),
})

const initialSearchValues: HuisartsZoekobject = leegHuisartsZoekobject

const HuisartsSelectieComponent: FC<HuisartsSelectieComponentProps> = ({
																		   description,
																		   huidigeHuisarts,
																		   geenHuisartsTekst,
																		   magOntkoppelen,
																		   onHuisartsGekozen,
																		   onHuisartsVerwijderen,
																		   onAnnulerenVerwijderen,
																		   onBevestigenVerwijderen,
																		   contactUrl,
																		   toonBlob,
																		   mammaHuidigeGeenHuisartsOptie,
																		   analyticsCategorie,
																	   }) => {
	const dispatch = useThunkDispatch()
	const [zoekObject, setZoekObject] = useState<HuisartsZoekobject>(leegHuisartsZoekobject)
	const [zoekResultaten, setZoekResultaten] = useState<Huisarts[]>(geenHuisartsZoekresultaten)
	const [gekozenHuisarts, setGekozenHuisarts] = useState<Huisarts>()
	const [gezocht, setGezocht] = useState(false)
	const [disableMeer, setDisableMeer] = useState(true)
	const [toonVerwijderPopup, setToonVerwijderPopup] = useState(false)
	const [isAdvancedSearch, setAdvancedSearch] = useState<boolean>(false)

	const geenResultaten = gezocht && zoekResultaten.length === 0

	useEffect(() => {
		setDisableMeer(zoekResultaten.length === 0 || zoekResultaten.length % 10 !== 0)
	}, [zoekResultaten])

	useEffect(() => {
		if (toonVerwijderPopup && !huidigeHuisarts && !mammaHuidigeGeenHuisartsOptie) {
			setToonVerwijderPopup(false)
			setGekozenHuisarts(undefined)
		}
	}, [huidigeHuisarts, mammaHuidigeGeenHuisartsOptie, toonVerwijderPopup])

	const zoekHuisartsen = useCallback(async (zoek: HuisartsZoekobject, pagina: number) => {
		const result = await dispatch(zoekHuisartsenAction(zoek, pagina))

		if (pagina === 0) {
			setZoekResultaten(result)
		} else {
			setZoekResultaten(prev => [...prev, ...result])
		}

		setGezocht(true)
	}, [dispatch])

	const zoek = (values: HuisartsZoekobject): void => {
		setZoekObject(values)
		setZoekResultaten(geenHuisartsZoekresultaten)
		setDisableMeer(false)
		zoekHuisartsen(values, 0)
	}

	const stuurDatadogEventEnHandleSubmit = (values: HuisartsZoekobject, handleSubmit: (e?: React.FormEvent<HTMLFormElement>) => void): void => {
		datadogService.stuurEvent("huisartsZoeken", analyticsCategorie, {
			praktijk: values.naam,
			plaats: values.adres.plaats,
			postcode: values.adres.postcode,
			straat: values.adres.straat,
		})
		handleSubmit()
	}

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

	const zoekMeer = (): void => {
		const pagina = zoekResultaten.length / 10
		zoekHuisartsen(zoekObject, pagina)
	}

	function kies(h: Huisarts): void {
		datadogService.stuurEvent("huisartsOptieGekozen", AnalyticsCategorie.MAMMA_HUISARTS)

		if (huidigeHuisarts && huidigeHuisarts.id === h.id) {
			showToast(getString(properties.gedeeld.toasts.zelfde.title), getString(properties.gedeeld.toasts.zelfde.description))
			return
		}

		setGekozenHuisarts(h)
	}

	return (
		<div>
			<Row>
				<Col md={huidigeHuisarts ? 8 : 12}>
					{description && (
						<p>
							<SpanWithHtml
								className={styles.infoText}
								value={description}/>
						</p>
					)}
				</Col>

				<Col md={4}>
					{toonBlob && (huidigeHuisarts || geenHuisartsTekst) && (
						<TextBlobComponent
							titel={getString(properties.gedeeld.blob.title)}
							tekst={
								(huidigeHuisarts &&
									concatWithSpace(
										huidigeHuisarts.voorletters,
										huidigeHuisarts.achternaam,
									)) ||
								getGeenHuisartsOptieTekst(mammaHuidigeGeenHuisartsOptie)
							}
							adresLocatie={
								huidigeHuisarts &&
								getString(properties.gedeeld.blob.locatie, [
									huidigeHuisarts.praktijknaam,
									concatWithSpace(
										huidigeHuisarts.adres.straat,
										huidigeHuisarts.adres.huisnummer,
									),
									concatWithSpace(
										huidigeHuisarts.adres.postcode,
										huidigeHuisarts.adres.plaats,
									),
								])
							}
							onLinkClick={() => {
								if (!magOntkoppelen) {
									showToast(
										getString(properties.gedeeld.toasts.geen.title),
										getString(properties.gedeeld.toasts.geen.description),
									)
								} else if (huidigeHuisarts || mammaHuidigeGeenHuisartsOptie) {
									datadogService.stuurEvent(
										"HuisartsVerwijderen",
										AnalyticsCategorie.MAMMA,
									)
									setToonVerwijderPopup(true)
									onHuisartsVerwijderen()
								}
							}}
							linkTekst={getString(
								properties.gedeeld.zoekpagina.blob.link,
							)}/>
					)}
				</Col>
			</Row>

			<div
				className={classNames(styles.childrenContainer, {
					[styles.zonderDescription]: !description,
				})}>
				<Row>
					<Col md={5}>
						<Formik<HuisartsZoekobject>
							initialValues={initialSearchValues}
							validationSchema={searchSchema}
							onSubmit={zoek}>
							{({errors, values, setFieldValue, handleSubmit}: FormikProps<HuisartsZoekobject>) => (
								<SearchForm title={getString(properties.gedeeld.zoekpagina.zoekform.title)}>
									<ScreenitTextfield
										value={values.naam ?? ""}
										name="naam"
										placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.naam)}
										onChange={v => setFieldValue("naam", v)}/>
									<ScreenitTextfield
										value={values.adres.plaats ?? ""}
										name="adres.plaats"
										placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.plaats)}
										onChange={v => setFieldValue("adres.plaats", v)}/>

									<div className={styles.advancedSearchButton}
										 onClick={async () => {
											 await setFieldValue("adres.postcode", "")
											 await setFieldValue("adres.straat", "")
											 setAdvancedSearch(!isAdvancedSearch)
										 }}>
										<AdvancedSearchLinkComponent
											advancedSearch={isAdvancedSearch}
											onClickStuurDatadogEvent={() => datadogService.stuurEvent("huisartsmeerZoekopties", analyticsCategorie)}/>
									</div>

									{isAdvancedSearch && <div>
										<ScreenitTextfield
											value={values.adres.postcode ?? ""}
											name="adres.postcode"
											placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.postcode)}
											invalidMessage={errors.adres?.postcode as unknown as string}
											onChange={v => setFieldValue("adres.postcode", v)}/>
										<ScreenitTextfield
											value={values.adres.straat ?? ""}
											name="adres.straat"
											placeholder={getString(properties.gedeeld.zoekpagina.zoekform.placeholders.straat)}
											onChange={v => setFieldValue("adres.straat", v)}/>
									</div>}
									<Button
										label={getString(properties.gedeeld.zoekpagina.zoekform.submit)}
										displayArrow={ArrowType.ARROW_RIGHT}
										className={bvoStyle.darkBackgroundColor}
										onClick={() => stuurDatadogEventEnHandleSubmit(values, handleSubmit)}/>
								</SearchForm>
							)}
						</Formik>
					</Col>
					<Col md={7} className={styles.results}>
						{geenResultaten &&
							<>
								<FormErrorComponent text={getString(properties.gedeeld.zoekpagina.resultaten.geen)}/>
								<BigUrlButton
									link={contactUrl}
									title={getString(properties.gedeeld.zoekpagina.resultaten.button.header)}
									text={getString(properties.gedeeld.zoekpagina.resultaten.button.text)}/>
							</>
						}
						{zoekResultaten.map(h =>
							<SearchResultHuisarts
								key={h.id}
								className={styles.results}
								col1={["", h.praktijknaam ?? "", concatWithSpace(h.voorletters, h.achternaam)]}
								col2={[getString(properties.gedeeld.zoekpagina.resultaten.resultaat.locatie_header), concatWithSpace(h.adres.straat, h.adres.huisnummer), concatWithSpace(h.adres.postcode, h.adres.plaats)]}
								onHoverText={getString(properties.gedeeld.zoekpagina.resultaten.resultaat.hover)}
								onClickAction={() => kies(h)}/>,
						)}
						{gezocht && !disableMeer && !geenResultaten && <div className={styles.showMoreResultsButtonArea}>
							<Button
								label={getString(properties.gedeeld.zoekpagina.resultaten.meer)}
								displayArrow={ArrowType.ARROW_DOWN}
								arrowBeforeLabel={false}
								onClick={zoekMeer}/>
						</div>}
					</Col>
				</Row>

				{gekozenHuisarts && (
					<HuisartsBevestigingsPopup
						huisarts={gekozenHuisarts}
						type={HuisartsBevestigingsPopupType.BEVESTIGEN}
						onPrimaireKnop={() => {
							onHuisartsGekozen(gekozenHuisarts)
							showToast(
								getString(properties.gedeeld.toasts.opgeslagen.title),
								getString(properties.gedeeld.toasts.opgeslagen.description),
							)
							setGekozenHuisarts(undefined)
						}}
						onSecundaireKnop={() => setGekozenHuisarts(undefined)}/>
				)}

				{toonVerwijderPopup && huidigeHuisarts && (
					<HuisartsBevestigingsPopup
						huisarts={huidigeHuisarts}
						type={HuisartsBevestigingsPopupType.VERWIJDEREN}
						onPrimaireKnop={() => {
							setToonVerwijderPopup(false)
							setGekozenHuisarts(undefined)
							onBevestigenVerwijderen()
						}}
						onSecundaireKnop={() => {
							setToonVerwijderPopup(false)
							onAnnulerenVerwijderen()
						}}/>
				)}
			</div>
		</div>
	)
}

export default HuisartsSelectieComponent
