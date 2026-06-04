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
import SpanWithHtml from "../../../../components/span/SpanWithHtml"
import Button from "../../../../components/input/Button"
import styles from "./ColonAfspraakWizard.module.scss"
import properties from "./ColonAfspraakWizard.json"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {useNavigate} from "react-router"
import ColonAfspraakWizard, {useAfspraakWizardContext} from "./ColonAfspraakWizard"
import {FC} from "react"
import classNames from "classnames"
import {NavLink} from "react-bootstrap"
import ScreenitTextfield from "../../../../components/input/ScreenitTextfield"
import {Formik} from "formik"
import * as Yup from "yup"
import {isEmailadresValid} from "../../../../validators/EmailValidator"
import {isMobielnummerValid} from "../../../../validators/TelefoonnummerValidator"
import {useSelector} from "react-redux"
import {selectPersoon} from "../../../../selectors/ClientSelectors"
import {useThunkDispatch} from "../../../../index"
import {saveEmail} from "../../../../api/EmailWijzigenThunkAction"
import {saveTelefoonNummers} from "../../../../api/TelefoonnummerWijzigenThunkAction"
import datadogService from "../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"

const ColonAfspraakContactPage: FC = () => {
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const persoon = useSelector(selectPersoon)
	const {openAfsluitenPopup} = useAfspraakWizardContext()

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		email: Yup.string().required(getString(properties.contact.email_validatie.niet_aanwezig))
			.test("mailValidatie", getString(properties.contact.email_validatie.fout), (value) => {
				return isEmailadresValid(value)
			})
			.max(100, getString(properties.contact.email_validatie.te_lang)),
		mobiel: Yup.string().required(getString(properties.contact.sms_validatie.niet_aanwezig))
			.test("telefoonnummerValidatie", getString(properties.contact.sms_validatie.fout), (value) => {
				return isMobielnummerValid(value)
			}),
	})

	const verstuurEventEnNavigate = async (values: { email: string, mobiel: string }): Promise<void> => {
		await dispatch(saveEmail({emailadres: values.email}))
		await dispatch(saveTelefoonNummers({telefoonnummer1: values.mobiel}))
		datadogService.stuurEvent(
			"volgendeGeklikt",
			AnalyticsCategorie.AFSPRAAK_VERZETTEN,
			{stap: 4},
		)
		navigate("/colon/afspraak/overzicht")
	}

	const initialValues = {
		mobiel: persoon.telefoonnummer1 || "",
		email: persoon.emailadres || "",
	}

	return (
		<>
			<Formik initialValues={initialValues}
			        validationSchema={validatieSchema}
			        onSubmit={values => verstuurEventEnNavigate(values)}>
				{({errors, values, setFieldValue, handleSubmit}) => (
					<>
						<div>
							<SpanWithHtml className={styles.infoText} value={properties.contact.description}/>
							<div>
								<ScreenitTextfield name={"email"}
								                   placeholder={"E-mailadres *"}
								                   value={values.email}
								                   invalidMessage={errors.email}
								                   onChange={value => setFieldValue("email", value)}/>
								<ScreenitTextfield
									name={"mobiel"}
									placeholder={"Mobiel nummer *"}
									value={values.mobiel}
									invalidMessage={errors.mobiel}
									onChange={value => setFieldValue("mobiel", value)}/>
							</div>
							<SpanWithHtml className={styles.infoText} value={properties.contact.uitleg}/>

							<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
								<Button lightStyle={true} displayArrow={ArrowType.ARROW_LEFT} onClick={() => navigate("/colon/afspraak/kiezen/")}
								        label={properties.afspraak_maken.button.vorige}/>
								<div className={styles.knoppenRechts}>
									<Button label={getString(properties.afspraak_maken.button.volgende)}
									        displayArrow={ArrowType.ARROW_RIGHT}
									        onClick={handleSubmit}/>
									<NavLink onClick={openAfsluitenPopup} className={styles.andereOptie}>
										{getString(properties.afspraak_maken.button.afsluiten)}</NavLink>
								</div>
							</div>
						</div>
					</>)}
			</Formik>
		</>
	)
}

const WrappedColonAfspraakContactPage: FC = () => (
	<ColonAfspraakWizard>
		<ColonAfspraakContactPage/>
	</ColonAfspraakWizard>
)

export default WrappedColonAfspraakContactPage
