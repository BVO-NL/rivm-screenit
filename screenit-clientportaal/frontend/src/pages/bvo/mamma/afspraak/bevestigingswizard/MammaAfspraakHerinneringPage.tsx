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
import {isMobielnummerValid} from "../../../../../validators/TelefoonnummerValidator"
import styles from "./MammaAfspraakBevestigingsWizard.module.scss"
import {Formik} from "formik"
import {useSelector} from "react-redux"
import {selectMammaAfspraakBevestigingsoptie} from "../../../../../selectors/MammaAfspraakSelectors"
import ScreenitTextfield from "../../../../../components/input/ScreenitTextfield"
import {useNavigate} from "react-router"
import datadogService from "../../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../../datatypes/AnalyticsCategorie"
import Button from "../../../../../components/input/Button"
import {ArrowType} from "../../../../../components/vectors/ArrowIconComponent"
import {useThunkDispatch} from "../../../../../index"
import {Checkbox} from "@mui/material"
import classNames from "classnames"
import AfsluitenLink from "../../../../../components/afsluiten_link/AfsluitenLink"
import {FC} from "react"
import SpanWithHtml from "../../../../../components/span/SpanWithHtml"
import {showToast} from "../../../../../utils/ToastUtil"
import {ToastMessageType} from "../../../../../datatypes/toast/ToastMessage"
import {maakAfspraakBevestiging} from "../../../../../api/MammaAfspraakMakenThunkAction"
import {useWizardStap} from "../../../../../components/wizard_indicator/WizardIndicatorContext"

const MammaAfspraakHerinneringPage: FC = () => {
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		clientNieuwMobielNummer: Yup.string()
			.when("wilHerinneringsSms", {
				is: (value: string) => value,
				then: schema => schema.required(getString(properties.sms.sms_validatie.niet_aanwezig))
					.test("telefoonnummerValidatie", getString(properties.sms.sms_validatie.fout), (value) => {
						return isMobielnummerValid(value)
					}),
				otherwise: schema => schema.notRequired(),
			}),
	})
	const afspraakBevestiging = useSelector(selectMammaAfspraakBevestigingsoptie)!
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const huidigeStap = useWizardStap()

	const stuurEventEnGaVerder = async (): Promise<void> => {
		verstuurEvent()
		await maakBevestiging()
		navigate("/mamma/afspraak/overzicht")
	}

	const verstuurEvent = (): void => {
		datadogService.stuurEvent("smsherinneringGestuurd", AnalyticsCategorie.MAMMA_AFSPRAAK, {
			naam: `${afspraakBevestiging.wilHerinneringsSms ? "wel" : "geen"} herinnering verstuurd`,
			stap: huidigeStap,
		})
	}

	const maakBevestiging = async (): Promise<void> => {
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
			<SpanWithHtml className={styles.infoText} value={properties.afspraak_maken.herinnering.description}/>
			<Formik
				initialValues={afspraakBevestiging}
				validationSchema={validatieSchema}
				onSubmit={stuurEventEnGaVerder}>
				{({errors, values, isSubmitting, setFieldValue, handleSubmit}) => (
					<>
						<Checkbox name={"wilHerinneringsSms"}
								  id={"herinneringsSms"}
								  data-testid={"checkbox_sms_herinnering"}
								  checked={values.wilHerinneringsSms}
								  onChange={e => setFieldValue("wilHerinneringsSms", e.target.checked)}/>
						<label className={styles.label}
							   htmlFor={"herinneringsSms"}>
							{properties.afspraak_maken.herinnering.stuur_herinnering}
						</label>
						<ScreenitTextfield
							name={"mobielnummer"}
							placeholder={"Mobiel nummer"}
							value={values.clientNieuwMobielNummer}
							invalidMessage={errors.clientNieuwMobielNummer}
							onChange={value => setFieldValue("clientNieuwMobielNummer", value)}
							disabled={!values.wilHerinneringsSms}/>

						<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
							<Button lightStyle={true} displayArrow={ArrowType.ARROW_LEFT} onClick={() => navigate("/mamma/afspraak/bevestiging-selectie/")}
									label={properties.afspraak_maken.button.vorige}/>
							<div className={styles.knoppenRechts}>
								<Button label={properties.afspraak_maken.button.volgende}
										disableButton={isSubmitting}
										onClick={() => {
											afspraakBevestiging.clientNieuwMobielNummer = values.clientNieuwMobielNummer
											afspraakBevestiging.wilHerinneringsSms = values.wilHerinneringsSms
											handleSubmit()
										}}
										displayArrow={ArrowType.ARROW_RIGHT}/>
								<AfsluitenLink onAfsluitenKlik={() => afspraakBevestiging.resetKeuzes()}/>
							</div>
						</div>
					</>)}
			</Formik>
		</div>
	)
}

const WrappedMammaAfspraakHerinneringPage: FC = () => (
	<MammaAfspraakBevestigingsWizard>
		<MammaAfspraakHerinneringPage/>
	</MammaAfspraakBevestigingsWizard>
)
export default WrappedMammaAfspraakHerinneringPage
