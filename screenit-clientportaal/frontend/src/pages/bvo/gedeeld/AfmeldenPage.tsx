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
import {useEffect, useState} from "react"
import ActieBasePage from "../../ActieBasePage"
import styles from "./AfmeldenPage.module.scss"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../../../datatypes/Bevolkingsonderzoek"
import {useThunkDispatch} from "../../../index"
import {saveNieuwAfmeldMoment} from "../../../api/AfmeldenThunkAction"
import {AfmeldType} from "../../../datatypes/afmelden/AfmeldType"
import {FormErrorComponent} from "../../../components/form_error/FormErrorComponent"
import {getString} from "../../../utils/TekstPropertyUtil"
import {splitEnumString} from "../../../utils/EnumUtil"
import {useSelectedBvo} from "../../../utils/Hooks"
import {Formik, FormikValues} from "formik"

import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import SubmitForm from "../../../components/form/SubmitForm"
import * as Yup from "yup"
import {AfmeldOptiesDto, geenAfmeldOpties} from "../../../datatypes/afmelden/AfmeldOptiesDto"
import ScreenitBackend from "../../../utils/Backend"
import LadenComponent from "../../../components/laden/LadenComponent"
import {AfmeldingDto} from "../../../datatypes/afmelden/AfmeldingDto"
import {CervixAfmeldingReden} from "../../../datatypes/afmelden/CervixAfmeldingReden"
import {getBvoBaseUrl} from "../../../utils/UrlUtil"
import {useNavigate} from "react-router"
import {showToast} from "../../../utils/ToastUtil"
import properties from "./AfmeldenPage.json"
import datadogService from "../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../datatypes/AnalyticsCategorie"

const AfmeldenPage = () => {
	const selectedBvo = useSelectedBvo()!
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const [afmeldOpties, setAfmeldOpties] = useState<AfmeldOptiesDto>(geenAfmeldOpties)

	useEffect(() => {
		ScreenitBackend.get<AfmeldOptiesDto>(`afmelden/${selectedBvo}`).json()
			.then((response) => setAfmeldOpties(response))
	}, [setAfmeldOpties, selectedBvo])

	const magEenmaligAfmelden = afmeldOpties.afmeldOpties.includes(AfmeldType.EENMALIG)
	const magTijdelijkAfmelden = afmeldOpties.afmeldOpties.includes(AfmeldType.TIJDELIJK)
	const magDefinitiefAfmelden = afmeldOpties.afmeldOpties.includes(AfmeldType.DEFINITIEF)

	const initialValues = {
		afmeldType: magDefinitiefAfmelden && !magEenmaligAfmelden ? AfmeldType.DEFINITIEF : undefined,
		afmeldReden: null,
		afmeldenTotJaartal: undefined,
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		afmeldType: Yup.string().required(getString(properties.form.error)),
		afmeldReden: (() => {
			switch (selectedBvo) {
				case Bevolkingsonderzoek.COLON:
					return Yup.string().nullable()

				case Bevolkingsonderzoek.CERVIX:
					return Yup.string().when("afmeldType", {
						is: AfmeldType.DEFINITIEF,
						then: (schema) => schema.nullable().required(getString(properties.form.error)),
					})

				case Bevolkingsonderzoek.MAMMA:
					return Yup.string().nullable().required(getString(properties.form.error))
			}
		})(),
		afmeldenTotJaartal: Yup.number().when("afmeldType", {
			is: AfmeldType.TIJDELIJK,
			then: (schema) => schema.required(getString(properties.form.uitlegJaartal)),
		}),
	})

	function getAfmeldRedenString(reden: string): string {
		switch (reden) {
			case "MammaAfmeldingReden.MEDISCHE_REDEN":
				return getString(properties["MammaAfmeldingReden.MEDISCHE_REDEN"])
			case "MammaAfmeldingReden.ANGST_VOOR_KANKER":
				return getString(properties["MammaAfmeldingReden.ANGST_VOOR_KANKER"])
			case "MammaAfmeldingReden.ANGST_VOOR_STRALING":
				return getString(properties["MammaAfmeldingReden.ANGST_VOOR_STRALING"])
			case "MammaAfmeldingReden.NARE_ERVARING_MET_ONDERZOEK":
				return getString(properties["MammaAfmeldingReden.NARE_ERVARING_MET_ONDERZOEK"])
			case "MammaAfmeldingReden.ONDERZOEK_NIET_NUTTIG":
				return getString(properties["MammaAfmeldingReden.ONDERZOEK_NIET_NUTTIG"])
			case "MammaAfmeldingReden.PIJNLIJKHEID_VORIG_ONDERZOEK":
				return getString(properties["MammaAfmeldingReden.PIJNLIJKHEID_VORIG_ONDERZOEK"])
			case "MammaAfmeldingReden.VERHINDERD":
				return getString(properties["MammaAfmeldingReden.VERHINDERD"])
			case "MammaAfmeldingReden.OVERIGE_REDENEN":
				return getString(properties["MammaAfmeldingReden.OVERIGE_REDENEN"])
			case "CervixAfmeldingReden.UE":
				return getString(properties["CervixAfmeldingReden.UE"])
			case "CervixAfmeldingReden.ANDERS":
				return getString(properties["CervixAfmeldingReden.ANDERS"])
			default:
				return ""
		}
	}

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[selectedBvo]}
			title={getString(properties.title.afmelden, [BevolkingsonderzoekNaam[selectedBvo]])}
			description={getString(properties.infoText)}
			hintEinde={getString(properties.hintText)}>

			{!magEenmaligAfmelden && !magDefinitiefAfmelden && !magTijdelijkAfmelden && <LadenComponent/>}

			<div className={styles.uitlegDiv}>
				<ul>
					{magEenmaligAfmelden && <div>
						<li>{getString(properties.afmeldType.eenmalig.text)}</li>
						<p>{getString(properties.afmeldType.eenmalig.info)}</p>
					</div>}
					{magTijdelijkAfmelden && <div>
						<li>{getString(properties.afmeldType.tijdelijk.text)}</li>
						<p>{getString(properties.afmeldType.tijdelijk.info)}</p>
					</div>}
					{magDefinitiefAfmelden && <div>
						<li>{getString(properties.afmeldType.definitief.text)}</li>
						<p>{getString(properties.afmeldType.definitief.info)}</p>
					</div>}
				</ul>
			</div>

			{afmeldOpties?.heeftOpenColonIntakeAfspraak && <FormErrorComponent text={getString(properties.errorIntakeAfspraak)}/>}

			{(magEenmaligAfmelden || magDefinitiefAfmelden || magTijdelijkAfmelden) &&
				<Formik initialValues={initialValues}
						enableReinitialize={true}
						validationSchema={validatieSchema}
						onSubmit={(values) => {
							if (selectedBvo === Bevolkingsonderzoek.MAMMA) {
								datadogService.stuurEvent("afgemeldOnderzoek", AnalyticsCategorie.MAMMA_AFMELDEN, {periode: values.afmeldType, reden: values.afmeldReden})
							}
							dispatch(saveNieuwAfmeldMoment(selectedBvo,
								getAfmeldingDto(values),
							)).then(() => {
								showToast(undefined,
									bepaalAfmeldingToastTekst(values.afmeldType))
								navigate(getBvoBaseUrl(selectedBvo))
							})
						}}>
					{formikProps => (
						<SubmitForm title={bepaalAfmeldFormulierTitel()}
									formikProps={formikProps}
									buttonLabel={getString(properties.form.submit)}>

							<FormControl variant="standard" required component="fieldset">

								<p data-testid={"error_geen_keuze_afmeldtype"} className={styles.errorLabel}>{formikProps.errors.afmeldType}</p>

								<RadioGroup
									className={styles.radiobuttons}
									name="afmeldType"
									onChange={formikProps.handleChange}
									value={formikProps.values.afmeldType || ""}
									onClick={() => formikProps.setFieldValue("afmeldReden", selectedBvo === Bevolkingsonderzoek.COLON ? null : "")}>
									<ul>
										{magEenmaligAfmelden && <li><FormControlLabel
											value={AfmeldType.EENMALIG}
											data-testid={"radio_eenmalig"}
											control={<Radio/>}
											label={getString(properties.form.radiobutton.eenmalig)}/></li>}
										{magTijdelijkAfmelden && <li><FormControlLabel
											value={AfmeldType.TIJDELIJK}
											data-testid={"radio_tijdelijk"}
											control={<Radio/>}
											label={getString(properties.form.radiobutton.tijdelijk)}/></li>}
										{magDefinitiefAfmelden && <li><FormControlLabel
											value={AfmeldType.DEFINITIEF}
											data-testid={"radio_definitief"}
											control={<Radio/>}
											label={getString(properties.form.radiobutton.definitief)}/></li>}
									</ul>
								</RadioGroup>

								{formikProps.values.afmeldType === AfmeldType.TIJDELIJK &&
									<div>
										<h3 className={styles.label}>{getString(properties.form.title.tijdelijkAfmeldenJaartal)}</h3>
										<p data-testid={"error_geen_keuze_tijdelijk_afmelden_jaartal"} className={styles.errorLabel}>{formikProps.errors.afmeldenTotJaartal}</p>
										<RadioGroup
											className={styles.radiobuttons}
											name="afmeldenTotJaartal"
											value={formikProps.values.afmeldenTotJaartal || ""}
											onChange={formikProps.handleChange}>
											<ul>
												{afmeldOpties.mogelijkeAfmeldJaren
													.map((jaar: any, index) => {
														return <li key={index}>
															<FormControlLabel key={index}
																			  data-testid={`radio_${  jaar}`}
																			  control={<Radio/>}
																			  className={styles.afmeldRedenRadioButton}
																			  value={jaar}
																			  label={jaar}/>
														</li>
													})}
											</ul>
										</RadioGroup>
									</div>}

								{(formikProps.values.afmeldType !== undefined || !magEenmaligAfmelden) && afmeldTypeHeeftAfmeldingRedenen(formikProps.values.afmeldType) && selectedBvo !== Bevolkingsonderzoek.COLON &&
									<div>
										<h3 className={styles.label}>{getString(properties.form.title.afmeldReden)}</h3>
										<p data-testid={"error_geen_keuze_afmeldreden"} className={styles.errorLabel}>{formikProps.errors.afmeldReden}</p>
										<RadioGroup
											className={styles.radiobuttons}
											name="afmeldReden"
											value={formikProps.values.afmeldReden || ""}
											onChange={formikProps.handleChange}>
											<ul>
												{getAfmeldRedenen(formikProps.values.afmeldType)
													.map((reden: string, index) => {
														return <li key={index}>
															<FormControlLabel key={index}
																			  data-testid={`radio_${  reden}`}
																			  control={<Radio/>}
																			  className={styles.afmeldRedenRadioButton}
																			  value={splitEnumString(reden) ?? ""}
																			  label={getAfmeldRedenString(reden)}/>
														</li>
													})}
											</ul>
										</RadioGroup>
									</div>}
							</FormControl>
						</SubmitForm>)}
				</Formik>}

		</ActieBasePage>
	)

	function getAfmeldingDto(values: FormikValues): AfmeldingDto {
		if (selectedBvo === Bevolkingsonderzoek.CERVIX && values.afmeldType === AfmeldType.EENMALIG) {
			return {
				afmeldType: values.afmeldType!,
				afmeldReden: CervixAfmeldingReden.ANDERS,
			}
		} else {
			return {
				afmeldType: values.afmeldType!,
				afmeldReden: values.afmeldReden!,
				afmeldenTotJaartal: values.afmeldenTotJaartal,
			}
		}
	}

	function bepaalAfmeldFormulierTitel(): string {
		if (magEenmaligAfmelden && magTijdelijkAfmelden) {
			return getString(properties.form.title.afmeldType.alleOptiesColon)
		} else if (magEenmaligAfmelden) {
			return getString(properties.form.title.afmeldType.algemeen)
		} else if (!magEenmaligAfmelden && magTijdelijkAfmelden) {
			return getString(properties.form.title.afmeldType.tijdelijkEnDefinitief)
		} else {
			return getString(properties.form.title.afmeldType.definitief)
		}
	}

	function afmeldTypeHeeftAfmeldingRedenen(afmeldType?: AfmeldType) {
		return !(selectedBvo === Bevolkingsonderzoek.CERVIX && afmeldType === AfmeldType.EENMALIG)
	}

	function getAfmeldRedenen(afmeldType?: AfmeldType) {
		if (AfmeldType.EENMALIG === afmeldType) {
			return afmeldOpties.afmeldRedenenEenmalig
		} else if (AfmeldType.TIJDELIJK === afmeldType) {
			return afmeldOpties.afmeldRedenenTijdelijk
		} else {
			return afmeldOpties.afmeldRedenenDefinitief
		}
	}

	function bepaalAfmeldingToastTekst(afmeldType?: AfmeldType): string {
		if (AfmeldType.EENMALIG === afmeldType) {
			if (selectedBvo === Bevolkingsonderzoek.MAMMA) {
				return getString(properties.toast.eenmalig_bk)
			} else {
				return getString(properties.toast.eenmalig_dk_bmhk, [BevolkingsonderzoekNaam[selectedBvo]])
			}
		} else if (AfmeldType.TIJDELIJK === afmeldType) {
			return getString(properties.toast.tijdelijk, [BevolkingsonderzoekNaam[selectedBvo]])
		} else {
			return getString(properties.toast.definitief, [BevolkingsonderzoekNaam[selectedBvo]])
		}
	}
}

export default AfmeldenPage
