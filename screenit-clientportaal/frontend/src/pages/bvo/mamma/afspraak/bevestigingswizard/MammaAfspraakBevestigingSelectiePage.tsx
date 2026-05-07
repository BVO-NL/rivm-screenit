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
import * as Yup from "yup"
import {getString} from "../../../../../utils/TekstPropertyUtil"
import properties from "./MammaAfspraakBevestigingsWizard.json"
import {isEmailadresValid} from "../../../../../validators/EmailValidator"
import styles from "./MammaAfspraakBevestigingsWizard.module.scss"
import {Formik} from "formik"
import {BevestigingsType} from "../../../../../datatypes/BevestigingsType"
import ScreenitTextfield from "../../../../../components/input/ScreenitTextfield"
import Button from "../../../../../components/input/Button"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import {useNavigate} from "react-router"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import {useSelector} from "react-redux"
import {selectMammaAfspraakBevestigingsoptie} from "../../../../../selectors/MammaAfspraakSelectors"
import {useThunkDispatch} from "../../../../../index"
import {FormControlLabel, Radio, RadioGroup} from "@mui/material"
import SpanWithHtml from "../../../../../components/span/SpanWithHtml"
import AfsluitenLink from "../../../../../components/afsluiten_link/AfsluitenLink"
import React, {FC} from "react"
import {showToast} from "../../../../../utils/ToastUtil"
import {ToastMessageType} from "../../../../../datatypes/toast/ToastMessage"
import {maakAfspraakBevestiging} from "../../../../../api/MammaAfspraakMakenThunkAction"
import {useWizardStap} from "../../../../../components/wizard_indicator/WizardIndicatorContext"

const MammaAfspraakBevestigingSelectiePage: FC = () => {
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		bevestigingsType: Yup.string().required(),
		clientNieuwEmailAdres: Yup.string()
			.when("bevestigingsType", {
				is: (val: string) => val === BevestigingsType.MAIL.toString(),
				then: schema => schema.required(getString(properties.mail.email_validatie.niet_aanwezig))
					.test("mailValidatie", getString(properties.mail.email_validatie.fout), (value) => {
						return isEmailadresValid(value)
					})
					.max(100, getString(properties.mail.email_validatie.te_lang)),
				otherwise: schema => schema.notRequired(),
			}),
	})
	const afspraakBevestiging = useSelector(selectMammaAfspraakBevestigingsoptie)!
	const initialValues = {
		...afspraakBevestiging,
		bevestigingsType: afspraakBevestiging.bevestigingsType?.toString() ?? "",
	}
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const huidigeStap = useWizardStap()

	const verstuurEventEnNavigate = async (): Promise<void> => {
		verstuurEvent()
		await maakBevestiging()
		navigeerNaarVolgendePagina()
	}

	const verstuurEvent = (): void => {
		let bevestigingsnaam: string
		switch (afspraakBevestiging.bevestigingsType) {
			case BevestigingsType.MAIL:
				bevestigingsnaam = "e-mail"
				break
			case BevestigingsType.BRIEF:
				bevestigingsnaam = "post"
				break
			default:
				bevestigingsnaam = "geen bevestiging"
		}

		const mailIngevuld = (afspraakBevestiging.clientNieuwEmailAdres !== undefined && afspraakBevestiging.clientNieuwEmailAdres !== "") ? "ingevuld" : "niet ingevuld"

		datadogService.stuurEvent("ontvangstBevestigingGestuurd", AnalyticsCategorie.MAMMA_AFSPRAAK, {
			naam: bevestigingsnaam,
			stap: huidigeStap,
			mail: mailIngevuld,
		})
	}

	const navigeerNaarVolgendePagina = (): void => {
		const url = afspraakBevestiging.toonSmsOptie ? "/mamma/afspraak/herinnering/" : "/mamma/afspraak/overzicht/"
		navigate(url)
	}

	const maakBevestiging = async (): Promise<void> => {
		if (afspraakBevestiging?.toonSmsOptie) {
			return
		}

		try {
			await dispatch(maakAfspraakBevestiging(afspraakBevestiging))
		} catch (error: any) {
			if (error.response.data === "afspraak.bevestiging.niet.mogelijk") {
				showToast(properties.afspraak_maken.toast.geen_bevestiging, properties.afspraak_maken.toast.error.algemeen, ToastMessageType.ERROR)
			}
		}
	}

	return (
		<div>
			<SpanWithHtml className={styles.infoText} value={getString(properties.afspraak_maken.bevestiging.keuze_vraag)}/>
			<Formik initialValues={initialValues}
					validationSchema={validatieSchema}
					onSubmit={verstuurEventEnNavigate}>
				{({errors, values, isSubmitting, setFieldValue, handleSubmit}) => (
					<>
						<RadioGroup
							className={styles.radiobuttons}
							name="bevestigingsType"
							onChange={event => setFieldValue("bevestigingsType", event.target.value)}
							value={values.bevestigingsType || ""}>
							<ul>
								<li>
									<FormControlLabel
										value={BevestigingsType.MAIL.toString()}
										data-testid={"radio_mail"}
										control={<Radio/>}
										label={getString(properties.afspraak_maken.bevestiging.per_mail)}/>
								</li>
								<li>
									<FormControlLabel
										value={BevestigingsType.BRIEF.toString()}
										data-testid={"radio_post"}
										control={<Radio/>}
										label={getString(properties.afspraak_maken.bevestiging.per_post)}/>
								</li>
								<li>
									<FormControlLabel
										value={BevestigingsType.GEEN.toString()}
										data-testid={"radio_geen"}
										control={<Radio/>}
										label={getString(properties.afspraak_maken.bevestiging.geen_bevestiging)}/>
								</li>
							</ul>
						</RadioGroup>

						{values.bevestigingsType === BevestigingsType.MAIL.toString() && (
							<div>
								<span>{getString(properties.afspraak_maken.bevestiging.controleer_email)}</span>
								<ScreenitTextfield name={"mail"}
												   placeholder={"E-mailadres"}
												   value={values.clientNieuwEmailAdres}
												   invalidMessage={errors.clientNieuwEmailAdres}
												   onChange={value => setFieldValue("clientNieuwEmailAdres", value)}/>
							</div>
						)}

						<div className={styles.bevestigenForm}>
							<Button label={properties.afspraak_maken.button.volgende}
									disableButton={isSubmitting}
									onClick={() => {
										afspraakBevestiging.clientNieuwEmailAdres = values.clientNieuwEmailAdres
										afspraakBevestiging.bevestigingsType = Number(values.bevestigingsType)
										handleSubmit()
									}}
									displayArrow={ArrowType.ARROW_RIGHT}/>
							<AfsluitenLink/>
						</div>
					</>)}
			</Formik>
		</div>
	)
}

const WrappedMammaAfspraakBevestigingSelectiePage: FC = () => (
	<MammaAfspraakBevestigingsWizard>
		<MammaAfspraakBevestigingSelectiePage/>
	</MammaAfspraakBevestigingsWizard>
)
export default WrappedMammaAfspraakBevestigingSelectiePage
