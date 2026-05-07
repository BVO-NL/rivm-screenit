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
import styles from "./OpenstaandeOnderzoekenPage.module.scss"
import {Container, NavLink} from "react-bootstrap"
import classNames from "classnames"
import KruimelpadComponent from "../../../../components/kruimelpad/KruimelpadComponent"
import bvoStyles from "../../../../components/BvoStyle.module.scss"
import {Formik} from "formik"
import * as Yup from "yup"
import {getString} from "../../../../utils/TekstPropertyUtil"
import properties from "./OpenstaandeOnderzoekenPage.json"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import {useNavigate} from "react-router"
import Button from "../../../../components/input/Button"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import VerticalDividerComponent from "../../../../components/vectors/VerticalDividerComponent"
import {useSelector} from "react-redux"
import {State} from "../../../../datatypes/State"
import {FC, useEffect} from "react"
import {showToast} from "../../../../utils/ToastUtil"
import {ToastMessageType} from "../../../../datatypes/toast/ToastMessage"
import {verwerkOpenstaandeUitnodigingen} from "../../../../api/OpenstaandeUitnodigingenThunkAction"
import HttpStatusCode from "../../../../datatypes/HttpStatus"
import {useThunkDispatch} from "../../../../index"
import datadogService from "../../../../services/DatadogService"
import {nu} from "../../../../utils/DateUtil"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"
import {Bevolkingsonderzoek} from "../../../../datatypes/Bevolkingsonderzoek"
import {HTTPError} from "ky"

type ColonKeuze = "AANVRAGEN_NIEUW" | "HEB_NOG"

type CervixKeuze = "AANVRAGEN_ZELFTEST" | "AANVRAGEN_BRIEF" | "HEB_NOG"

const OpenstaandeOnderzoekenPage: FC = () => {
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const openstaandeUitnodigingen = useSelector((state: State) => state.client.openstaandeUitnodigingen)

	useEffect(() => {
		if (openstaandeUitnodigingen.length === 0) {
			navigate("/mamma/")
		}
	}, [navigate, openstaandeUitnodigingen.length])

	const openstaandeColonUitnodiging = openstaandeUitnodigingen.find(uitnodiging => uitnodiging.bevolkingsonderzoekType === Bevolkingsonderzoek.COLON)
	const openstaandeCervixUitnodiging = openstaandeUitnodigingen.find(uitnodiging => uitnodiging.bevolkingsonderzoekType === Bevolkingsonderzoek.CERVIX)

	const heeftOpenstaandeColon = !!openstaandeColonUitnodiging
	const heeftOpenstaandeCervix = !!openstaandeCervixUitnodiging

	type FormValues = {
		heeftOpenstaandeColon: boolean
		heeftOpenstaandeCervix: boolean
		colonKeuze?: ColonKeuze
		cervixKeuze?: CervixKeuze
	}

	const initialValues: FormValues = {
		heeftOpenstaandeColon,
		heeftOpenstaandeCervix,
		colonKeuze: undefined,
		cervixKeuze: undefined,
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		colonKeuze: Yup.string().when("heeftOpenstaandeColon", {
			is: (val: boolean) => val,
			then: schema => schema.required(getString(properties.colon.validatie)),
			otherwise: schema => schema.notRequired(),
		}),
		cervixKeuze: Yup.string().when("heeftOpenstaandeCervix", {
			is: (val: boolean) => val,
			then: schema => schema.required(getString(properties.cervix.validatie)),
			otherwise: schema => schema.notRequired(),
		}),
	})

	async function handleSubmit(values: FormValues): Promise<void> {
		datadogService.stuurEvent("afrondenGeklikt", AnalyticsCategorie.BVO_MIJN_ONDERZOEKEN, {
			naam: `dk: ${values.colonKeuze}, bmhk: ${values.cervixKeuze}`,
		})
		try {
			await dispatch(verwerkOpenstaandeUitnodigingen({
				colonKeuze: values.colonKeuze,
				cervixKeuze: values.cervixKeuze,
			}))

			const teksten = bepaalBevestigingsmelding(values.colonKeuze, values.cervixKeuze)
			showToast(getString(teksten.title), getString(teksten.description))
			navigate("/")
		} catch (error) {
			if (error instanceof HTTPError && error.response.status === HttpStatusCode.UNPROCESSABLE_ENTITY) {
				const foutSleutel = await error.response.text()
				const foutSleutelTrimmed = foutSleutel.replace(/^"|"$/g, "")
				const specifiekeFout = properties.toast.fout[foutSleutelTrimmed as keyof typeof properties.toast.fout]

				if (specifiekeFout && typeof specifiekeFout === "object" && "title" in specifiekeFout) {
					showToast(getString(specifiekeFout.title), getString(specifiekeFout.description), ToastMessageType.ERROR)
				} else {
					showToast(getString(properties.toast.fout.title), getString(properties.toast.fout.description), ToastMessageType.ERROR)
				}
			}
		}
	}

	const bepaalBevestigingsmelding = (colonKeuze?: ColonKeuze, cervixKeuze?: CervixKeuze): { title: string, description: string } => {
		let teksten = properties.toast.succes
		if (colonKeuze === "AANVRAGEN_NIEUW") {
			teksten = properties.toast.colon.nieuweTest
			if (cervixKeuze === "AANVRAGEN_ZELFTEST") {
				teksten = properties.toast.gecombineerd.nieuweTest
			} else if (cervixKeuze === "AANVRAGEN_BRIEF") {
				teksten = properties.toast.gecombineerd.nieuweBrief
			}
		} else if (cervixKeuze === "AANVRAGEN_ZELFTEST") {
			teksten = properties.toast.cervix.nieuweTest
		} else if (cervixKeuze === "AANVRAGEN_BRIEF") {
			teksten = properties.toast.cervix.nieuweBrief
		}
		return teksten
	}

	return (
		<Container fluid className={classNames(styles.content)}>
			<KruimelpadComponent/>
			<div>
				<h4 className={classNames(bvoStyles.bvoText)}>{getString(properties.intro.titel)}</h4>
				<h1 className={styles.bvoNaam}>{getString(properties.page.titel)}</h1>
			</div>
			<p>{getString(properties.intro.omschrijving)}</p>

			<Formik
				initialValues={initialValues}
				validationSchema={validatieSchema}
				onSubmit={handleSubmit}>
				{formikProps => (
					<form>
						{heeftOpenstaandeColon ? (
							<>
								<h3>{getString(properties.colon.titel)}</h3>
								<p>{getString(properties.colon.omschrijving)}</p>
								{isNogBruikbaar(openstaandeColonUitnodiging?.vervaldatum) ? (
										<p>
											{getString(properties.colon.heeftVervaldatumTekst)}
										</p>
									) :
									(
										<p>
											{getString(properties.colon.testVerlopenTekst)}
										</p>
									)
								}
								<p>{getString(properties.colon.keuzeTitel)}</p>

								<div className={styles.kaart}>
									<VerticalDividerComponent className={styles.verticalRectangle}/>

									<FormControl variant="standard" required component="fieldset">
										<RadioGroup
											name="colonKeuze"
											onChange={formikProps.handleChange}
											value={formikProps.values.colonKeuze ?? ""}>
											<ul className={styles.radioLijst}>
												<li>
													<FormControlLabel
														value="AANVRAGEN_NIEUW"
														data-testid="radio_colon_nieuwe_fit_aanvragen"
														control={<Radio/>}
														label={getString(properties.colon.opties.AANVRAGEN_NIEUW)}/>
												</li>
												{isNogBruikbaar(openstaandeColonUitnodiging?.vervaldatum) ? (
													<li>
														<FormControlLabel
															value="HEB_NOG"
															data-testid="radio_colon_fit_nog_houdbaar"
															control={<Radio/>}
															label={getString(properties.colon.opties.HEB_NOG)}/>
													</li>
												) : <li>
													<FormControlLabel
														value="HEB_NOG"
														data-testid="radio_colon_fit_niet_houdbaar"
														control={<Radio/>}
														label={getString(properties.colon.opties.HEB_NOG_VERLOPEN)}/>
												</li>}
											</ul>
										</RadioGroup>
									</FormControl>
								</div>

								{formikProps.errors.colonKeuze && <p data-testid="error_colonKeuze" className={styles.errorLabel}>{formikProps.errors.colonKeuze}</p>}
							</>
						) : null}

						{heeftOpenstaandeCervix ? (
							<>
								<br/>
								<h3>{getString(properties.cervix.titel)}</h3>
								<p>{getString(properties.cervix.omschrijving)}</p>
								{isNogBruikbaar(openstaandeCervixUitnodiging?.vervaldatum) ? (
										<p>
											{getString(properties.cervix.heeftVervaldatumTekst)}
										</p>
									) :
									(
										<p>
											{getString(properties.cervix.testVerlopenTekst)}
										</p>
									)
								}
								<p>{getString(properties.cervix.keuzeTitel)}</p>

								<div className={styles.kaart}>
									<VerticalDividerComponent className={styles.verticalRectangle}/>

									<FormControl variant="standard" required component="fieldset">
										<RadioGroup
											name="cervixKeuze"
											onChange={formikProps.handleChange}
											value={formikProps.values.cervixKeuze ?? ""}>
											<ul className={styles.radioLijst}>
												<li>
													<FormControlLabel
														value="AANVRAGEN_ZELFTEST"
														data-testid="radio_cervix_nieuwe_zas_aanvragen"
														control={<Radio/>}
														label={getString(properties.cervix.opties.AANVRAGEN_ZELFTEST)}/>
												</li>
												<li>
													<FormControlLabel
														value="AANVRAGEN_BRIEF"
														data-testid="radio_cervix_nieuwe-brief-aanvragen"
														control={<Radio/>}
														label={getString(properties.cervix.opties.AANVRAGEN_BRIEF)}/>
												</li>
												{isNogBruikbaar(openstaandeCervixUitnodiging?.vervaldatum) ? (
													<li>
														<FormControlLabel
															value="HEB_NOG"
															data-testid="radio_cervix_zas_nog_houdbaar"
															control={<Radio/>}
															label={getString(properties.cervix.opties.HEB_NOG)}/>
													</li>
												) : <li>
													<FormControlLabel
														value="HEB_NOG"
														data-testid={"radio_cervix_zas_niet_houdbaar"}
														control={<Radio/>}
														label={getString(properties.cervix.opties.HEB_NOG_VERLOPEN)}/>
												</li>}
											</ul>
										</RadioGroup>
									</FormControl>
								</div>

								{formikProps.errors.cervixKeuze && <p data-testid="error_cervixKeuze" className={styles.errorLabel}>{formikProps.errors.cervixKeuze}</p>}
							</>
						) : null}

						<div className={styles.acties}>
							<NavLink
								onClick={() => navigate("/mamma/")}
								className={styles.afsluitenLink}>
								{getString(properties.knoppen.afsluiten)}
							</NavLink>
							<Button
								className={bvoStyles.baseBackgroundColor}
								disableButton={formikProps.isSubmitting}
								label={getString(properties.knoppen.afronden)}
								displayArrow={ArrowType.ARROW_RIGHT}
								onClick={formikProps.handleSubmit}/>
						</div>
					</form>
				)}
			</Formik>
		</Container>
	)
}

function isNogBruikbaar(vervaldatum: Date | undefined): boolean {
	if (!vervaldatum) {
		return false
	}
	return nu() < new Date(vervaldatum)
}

export default OpenstaandeOnderzoekenPage
