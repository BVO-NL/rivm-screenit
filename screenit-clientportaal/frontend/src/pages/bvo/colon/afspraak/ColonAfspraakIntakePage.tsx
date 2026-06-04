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
import Button from "../../../../components/input/Button"
import styles from "./ColonAfspraakWizard.module.scss"
import properties from "./ColonAfspraakWizard.json"
import {getString} from "../../../../utils/TekstPropertyUtil"
import {ArrowType} from "../../../../components/vectors/ArrowIconComponent"
import {useNavigate} from "react-router"
import ColonAfspraakWizard, {useAfspraakWizardContext} from "./ColonAfspraakWizard"
import {FC} from "react"
import {Formik} from "formik"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import * as Yup from "yup"
import {ColonIntakeafspraakType} from "../../../../datatypes/colon/ColonIntakeafspraakType"
import {useThunkDispatch} from "../../../../index"
import {setColonIntakeafspraakAction, setColonIntakeafspraakTypeAction} from "../../../../actions/ColonDossierAction"
import datadogService from "../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"
import {NavLink} from "react-bootstrap"
import {selectIntakeAfspraak} from "../../../../selectors/ColonSelectors"
import {useSelector} from "react-redux"

const ColonAfspraakIntakePage: FC = () => {
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const {openAfsluitenPopup} = useAfspraakWizardContext()
	const intakeAfspraak = useSelector(selectIntakeAfspraak)

	type IntakeFormulierWaarden = {
		intake: ColonIntakeafspraakType | undefined
	}

	const initialValues: IntakeFormulierWaarden = {
		intake: undefined,
	}
	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		intake: Yup.string().required(getString(properties.intake.error)),
	})

	const stuurEventEnGaVerder = async (values: IntakeFormulierWaarden): Promise<void> => {
		dispatch(setColonIntakeafspraakTypeAction(values.intake as ColonIntakeafspraakType))

		datadogService.stuurEvent(
			"volgendeGeklikt",
			AnalyticsCategorie.AFSPRAAK_VERZETTEN,
			{stap: 1, type: values.intake})

		navigate("/colon/afspraak/uw-gezondheid")
	}

	return (
		<div>
			<Formik<IntakeFormulierWaarden>
				initialValues={initialValues}
				validationSchema={validatieSchema}
				onSubmit={stuurEventEnGaVerder}>
				{({setFieldValue, errors, handleSubmit, values}) => (
					<>
						<FormControl variant="standard" component="fieldset">

							<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{errors.intake}</p>

							<span className={styles.description}>{getString(properties.intake.description)}</span>
							<RadioGroup
								name="intake"
								onChange={event => setFieldValue("intake", event.target.value)}
								value={values.intake || ""}>
								<ul className={styles.radiobuttons}>
									<li className={styles.radioOptie}>
										<div className={styles.radioOptieLabel}>{getString(properties.intake.op_locatie.label)}</div>
										<FormControlLabel
											value={ColonIntakeafspraakType.OP_LOCATIE}
											data-testid={"radio_op_locatie"}
											control={<Radio/>}
											label={getString(properties.intake.op_locatie.description)}
											className={styles.radioOptieBeschrijving}
										/>
									</li>
									<li className={styles.radioOptie}>
										<div className={styles.radioOptieLabel}>{getString(properties.intake.digitaal.label)}</div>
										<FormControlLabel
											value={ColonIntakeafspraakType.DIGITAAL}
											data-testid={"radio_digitaal"}
											control={<Radio/>}
											label={getString(properties.intake.digitaal.description)}
											className={styles.radioOptieBeschrijving}/>
									</li>
								</ul>
							</RadioGroup>
						</FormControl>
						<div className={styles.bevestigenForm}>
							<Button label={getString(properties.afspraak_maken.button.volgende)}
							        displayArrow={ArrowType.ARROW_RIGHT}
							        onClick={() => handleSubmit()}/>
							<NavLink onClick={openAfsluitenPopup} className={styles.andereOptie}>
								{getString(properties.afspraak_maken.button.afsluiten)}</NavLink>
						</div>
					</>
				)}
			</Formik>
		</div>
	)
}

const WrappedColonAfspraakIntakePage: FC = () => (
	<ColonAfspraakWizard>
		<ColonAfspraakIntakePage/>
	</ColonAfspraakWizard>
)

export default WrappedColonAfspraakIntakePage
