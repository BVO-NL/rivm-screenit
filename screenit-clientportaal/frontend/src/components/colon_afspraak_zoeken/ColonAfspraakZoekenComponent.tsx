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
import {FC, useCallback, useEffect, useRef, useState} from "react"
import {Col, Row} from "react-bootstrap"
import styles from "./ColonAfspraakZoekenComponent.module.scss"
import {Formik} from "formik"
import SearchForm from "../form/SearchForm"
import {getString} from "../../utils/TekstPropertyUtil"
import properties from "./ColonAfspraakZoekenComponent.json"
import ScreenitTextfield from "../input/ScreenitTextfield"
import ScreenitDatePicker from "../input/ScreenitDatePicker"
import AdvancedSearchLinkComponent from "../form/AdvancedSearchLinkComponent"
import ScreenitDropdown, {DropdownOption} from "../input/ScreenitDropdown"
import Button from "../input/Button"
import bvoStyle from "../BvoStyle.module.scss"
import {ArrowType} from "../vectors/ArrowIconComponent"
import {placeNonBreakingSpaceInDate} from "../../utils/StringUtil"
import {formatDate, formatDateWithDayName, formatTime, formatWeekRange, getWeekNummer, plusDagen, plusMaanden, plusWerkdagen, vandaag} from "../../utils/DateUtil"
import {FormErrorComponent} from "../form_error/FormErrorComponent"
import BigUrlButton from "../bigUrlButton/BigUrlButton"
import {getContactUrl} from "../../utils/UrlUtil"
import BeforeSearching from "../search_results/BeforeSearching"
import * as Yup from "yup"
import ScreenitBackend from "../../utils/Backend"
import {VrijSlotZonderKamer} from "../../datatypes/VrijSlotZonderKamer"
import {createShowToastAction} from "../../actions/ToastAction"
import {ToastMessageType} from "../../datatypes/toast/ToastMessage"
import {useThunkDispatch} from "../../index"
import {useWindowDimensions} from "../../utils/Hooks"
import SearchResultDigitaleIntakeAfspraken from "../search_results/SearchResultDigitaleIntakeAfspraken"
import SearchResultAfspraken from "../search_results/SearchResultAfspraken"
import {useSelector} from "react-redux"
import {selectHeeftAsaScoreBovenDrie, selectIntakeafspraakType} from "../../selectors/ColonSelectors"
import {ColonIntakeafspraakType} from "../../datatypes/colon/ColonIntakeafspraakType"
import datadogService from "../../services/DatadogService"
import {AnalyticsCategorie} from "../../datatypes/AnalyticsCategorie"
import {ColonHeeftAsaScoreBovenDrie} from "../../datatypes/colon/ColonHeeftAsaScoreBovenDrie"

export interface ColonAfspraakZoekenComponentProps {
	onAfspraakGeselecteerd: (afspraak: VrijSlotZonderKamer) => void
}

export type VrijSlotZonderKamerFilter = {
	ziekenhuisnaam?: string,
	vanaf?: Date,
	totEnMet?: Date,
	plaats?: string,
	pagecount: number
	afstand?: number
	maxResultsPerSearchInteration?: number
}

const ColonAfspraakZoekenComponent: FC<ColonAfspraakZoekenComponentProps> = ({onAfspraakGeselecteerd}) => {
	const dispatch = useThunkDispatch()
	const {width} = useWindowDimensions()
	const geselecteerdeAfspraakType = useSelector(selectIntakeafspraakType)
	const asaScoreBovenDrie = useSelector(selectHeeftAsaScoreBovenDrie)

	const [zoekFilter, setZoekFilter] = useState<VrijSlotZonderKamerFilter>({
		pagecount: 1,
	})
	const [vrijeSloten, setVrijeSloten] = useState<VrijSlotZonderKamer[]>([])
	const [gezocht, setGezocht] = useState<boolean>(false)
	const [isAdvancedSearch, setAdvancedSearch] = useState<boolean>(false)
	const [isZoekMeerKnopZichtbaar, setIsZoekMeerKnopZichtbaar] = useState<boolean>(true)

	const gevondenAfsprakenDiv = useRef<HTMLDivElement | null>(null)
	const geenResultaten = vrijeSloten.length === 0 && gezocht
	const maxResultsPerSearchInteration = 10

	const getMinimumSearchDate = (): Date => {
		return plusWerkdagen(vandaag(), 3)
	}

	const afstandOpties = (): DropdownOption[] => {
		const afstanden = ["5", "10", "15", "20", "25", "30", "35", "40", "45"]
		const afstandOpties: DropdownOption[] = []
		for (const afstand of afstanden) {
			afstandOpties.push({value: afstand, label: `${afstand} km`})
		}
		return afstandOpties
	}

	const kiesAfspraakOptie = (kiesbareLocatie: VrijSlotZonderKamer, zoekFilter: VrijSlotZonderKamerFilter): void => {
		datadogService.stuurEvent(
			"afspraakoptieGekozen",
			AnalyticsCategorie.AFSPRAAK_VERZETTEN,
			{stap: 3, type: geselecteerdeAfspraakType},
		)

		setZoekFilter(zoekFilter)
		onAfspraakGeselecteerd(kiesbareLocatie)
	}

	useEffect(() => {
		setIsZoekMeerKnopZichtbaar(vrijeSloten.length === zoekFilter.pagecount * maxResultsPerSearchInteration)
	}, [setIsZoekMeerKnopZichtbaar, vrijeSloten, zoekFilter.pagecount])

	const validationSchema: Yup.AnyObjectSchema = Yup.object()
		.shape({
			vanaf: Yup.date().required(getString(properties.searchitems.date_from.error.verplicht))
				.nullable()
				.typeError(getString(properties.searchitems.date_from.error.type))
				.min(getMinimumSearchDate(), getString(properties.searchitems.date_from.error.date_too_early, [formatDate(getMinimumSearchDate())])),
			totEnMet: Yup.date().required(getString(properties.searchitems.date_from.error.verplicht))
				.nullable()
				.typeError(getString(properties.searchitems.date_from.error.type))
				.min(plusDagen(getMinimumSearchDate(), 1), getString(properties.searchitems.date_until.error_date_too_early, [formatDate(plusDagen(getMinimumSearchDate(), 1))])),
		})

	const initialValues = {
		ziekenhuisnaam: undefined,
		vanaf: getMinimumSearchDate(),
		totEnMet: plusMaanden(getMinimumSearchDate(), 1),
		plaats: undefined,
		pagecount: 1,
		afstand: undefined,
		maxResultsPerSearchInteration: maxResultsPerSearchInteration,
	}

	const zoekAfspraken = useCallback((filter: VrijSlotZonderKamerFilter) => {
		setZoekFilter(filter)
		return ScreenitBackend.post<VrijSlotZonderKamer[]>("colon/afspraak/zoeken", {
			json: {
				...filter,
				digitaleAfspraak: geselecteerdeAfspraakType === ColonIntakeafspraakType.DIGITAAL,
				asaScoreBovenDrie: asaScoreBovenDrie !== ColonHeeftAsaScoreBovenDrie.NEE,
			},
		}).json()
			.then((response: VrijSlotZonderKamer[]) => setVrijeSloten(response))
			.catch(() => {
				dispatch(createShowToastAction({
					title: getString(properties.toast.errors.zoeken.title),
					description: getString(properties.toast.errors.zoeken.message),
					type: ToastMessageType.ERROR,
					alGetoond: false,
				}))
			})
			.finally(() => {
				setGezocht(true)
			})
	}, [dispatch])

	const handleSubmit = async (values: VrijSlotZonderKamerFilter): Promise<void> => {
		datadogService.stuurEvent("zoekAfspraken", AnalyticsCategorie.COLON_AFSPRAAK, {
			datum: values.vanaf,
			plaats: values.plaats,
			reisafstand: values.afstand,
		})

		await zoekAfspraken(values as VrijSlotZonderKamerFilter)
		if (width <= 768 && gevondenAfsprakenDiv.current) {
			window.scrollTo(0, gevondenAfsprakenDiv.current.offsetTop - 100)
		}
	}

	const zoekDigitaleIntakeAfspraken = (): boolean => {
		return geselecteerdeAfspraakType === ColonIntakeafspraakType.DIGITAAL
	}

	return (
		<>
			<Row className={styles.style}>
				<Col md={5}>
					<Formik initialValues={initialValues}
					        validationSchema={validationSchema}
					        onSubmit={handleSubmit}>
						{({errors, values, setFieldValue, handleSubmit}) => (
							<>
								<SearchForm title={getString(properties.search.title)}>
									<ScreenitTextfield onChange={async value => {
										await setFieldValue("plaats", value)
										await setFieldValue("afstand", "")
									}}
									                   value={values.plaats}
									                   invalidMessage={errors.plaats}
									                   name={"plaats"}
									                   placeholder={getString(properties.searchitems.plaats.placeholder)}/>

									<ScreenitTextfield onChange={async value => {
										await setFieldValue("ziekenhuisnaam", value)
										await setFieldValue("afstand", "")
									}}
									                   value={values.ziekenhuisnaam}
									                   invalidMessage={errors.ziekenhuisnaam}
									                   name={"ziekenhuisnaam"}
									                   placeholder={getString(properties.searchitems.ziekenhuis.placeholder)}/>

									<ScreenitDatePicker className={styles.datepicker}
									                    propertyName={"vanaf"}
									                    label={getString(properties.searchitems.date_from.placeholder)}
									                    value={values.vanaf}
									                    errorLabel={errors.vanaf}
									                    onChange={value => setFieldValue("vanaf", value)}/>
									<ScreenitDatePicker className={styles.datepicker}
									                    propertyName={"totEnMet"}
									                    label={getString(properties.searchitems.date_until.placeholder)}
									                    value={values.totEnMet}
									                    errorLabel={errors.totEnMet}
									                    onChange={value => setFieldValue("totEnMet", value)}/>
									<div>
										<div className={styles.advancedSearchButton}
										     onClick={async () => {
												 await setFieldValue("afstand", "")
												 setAdvancedSearch(!isAdvancedSearch)
											 }}>
											<AdvancedSearchLinkComponent advancedSearch={isAdvancedSearch}
											                             onClickStuurDatadogEvent={() => datadogService.stuurEvent("meerzoekopties", AnalyticsCategorie.COLON_AFSPRAAK)}/>
										</div>
									</div>
									{isAdvancedSearch &&
										<ScreenitDropdown propertyName={"afstand"}
										                  invalidMessage={errors.afstand}
										                  value={values.afstand}
										                  options={afstandOpties()}
										                  placeholder={getString(properties.searchitems.afstand.placeholder)}
										                  onChange={async (event) => {
															  await setFieldValue("afstand", event.target.value)
															  await setFieldValue("plaats", "")
															  await setFieldValue("ziekenhuisnaam", "")
														  }}/>}
									<Button className={bvoStyle.darkBackgroundColor}
									        label={getString(properties.search.do_search)}
									        displayArrow={ArrowType.ARROW_RIGHT}
									        onClick={handleSubmit}/>
								</SearchForm>
							</>
						)}
					</Formik>
				</Col>
				<Col md={7} className={styles.results} ref={gevondenAfsprakenDiv}>
					{gezocht && vrijeSloten.map((kiesbareLocatie, index) =>
						zoekDigitaleIntakeAfspraken() ? <SearchResultDigitaleIntakeAfspraken
							key={index}
							className={styles.result}
							enlargeText
							col1={[getString(properties.resultaten.week_header, [getWeekNummer(kiesbareLocatie.startTijd)]), formatWeekRange(kiesbareLocatie.startTijd)]}
							col2={[getString(properties.resultaten.digitale_intake_locatie_header), kiesbareLocatie.ziekenhuis]}
							onHoverText={getString(properties.search.hovertext_digitaal)}
							onClickAction={() => kiesAfspraakOptie(kiesbareLocatie, zoekFilter)}
						/> : <SearchResultAfspraken
							key={index}
							className={styles.result}
							enlargeText

							col1={["", placeNonBreakingSpaceInDate(formatDateWithDayName(kiesbareLocatie.startTijd)), getString(properties.search.result.tijdstip, [formatTime(kiesbareLocatie.startTijd)])]}
							col2={[getString(properties.resultaten.locatie_header), kiesbareLocatie.ziekenhuis]}
							col3={[getString(properties.resultaten.adres_header), getString(properties.search.result.locatie, [kiesbareLocatie.adres, kiesbareLocatie.postcode, kiesbareLocatie.plaats])]}

							onHoverText={getString(properties.search.hovertext)}
							onClickAction={() => kiesAfspraakOptie(kiesbareLocatie, zoekFilter)}
						/>,
					)}
					{geenResultaten && <FormErrorComponent text={getString(properties.search.search_no_results)}/>}
					{geenResultaten &&
						<BigUrlButton title={getString(properties.search.search_no_results_contact_header)}
						              text={getString(properties.search.search_no_results_contact_text)}
						              link={getContactUrl()}/>}
					<div className={styles.navigationButtons}>
						{gezocht && !geenResultaten && isZoekMeerKnopZichtbaar &&
							<div className={styles.showMoreResultsButtonArea}>
								<Button
									label={getString(properties.search.navigation_more_results)}
									displayArrow={ArrowType.ARROW_DOWN}
									onClick={() => {
										const nieuwZoekFilter = {
											...zoekFilter,
										}
										nieuwZoekFilter.pagecount = nieuwZoekFilter.pagecount + 1
										setGezocht(true)
										zoekAfspraken(nieuwZoekFilter)
									}}/>
							</div>}
					</div>
					{!gezocht &&
						<BeforeSearching text={getString(properties.search.before_search)}/>}
				</Col>
			</Row>
		</>
	)
}

export default ColonAfspraakZoekenComponent
