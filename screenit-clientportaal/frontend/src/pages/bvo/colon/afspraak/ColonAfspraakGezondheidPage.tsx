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
import {Formik} from "formik"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import * as Yup from "yup"
import {ColonHeeftAsaScoreBovenDrie} from "../../../../datatypes/colon/ColonHeeftAsaScoreBovenDrie"
import {useThunkDispatch} from "../../../../index"
import {setColonHeeftAsaScoreBovenDrieAction} from "../../../../actions/ColonDossierAction"
import {useSelector} from "react-redux"
import {selectHeeftAsaScoreBovenDrie} from "../../../../selectors/ColonSelectors"
import {NavLink} from "react-bootstrap"
import datadogService from "../../../../services/DatadogService"
import {AnalyticsCategorie} from "../../../../datatypes/AnalyticsCategorie"

const gezondheidProperties = properties["uw-gezondheid"]

type GezondheidFormulierWaarden = {
	antwoord: string
}

const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
	antwoord: Yup.string().required(getString(gezondheidProperties.error)),
})

const ColonAfspraakGezondheidPage: FC = () => {
	const navigate = useNavigate()
	const dispatch = useThunkDispatch()
	const opgeslagenAntwoord = useSelector(selectHeeftAsaScoreBovenDrie)
	const {openAfsluitenPopup} = useAfspraakWizardContext()

	const initialValues: GezondheidFormulierWaarden = {
		antwoord: opgeslagenAntwoord ?? "",
	}

	const slaAntwoordOpEnGaVerder = (values: GezondheidFormulierWaarden): void => {
		dispatch(setColonHeeftAsaScoreBovenDrieAction(values.antwoord as ColonHeeftAsaScoreBovenDrie))

		datadogService.stuurEvent(
			"volgendeGeklikt",
			AnalyticsCategorie.AFSPRAAK_VERZETTEN,
			{stap: 2})

		navigate("/colon/afspraak/kiezen/")
	}

	return (
		<div>
			<SpanWithHtml className={styles.infoText} value={gezondheidProperties.description}/>
			<Formik<GezondheidFormulierWaarden>
				initialValues={initialValues}
				validationSchema={validatieSchema}
				onSubmit={slaAntwoordOpEnGaVerder}>
				{({setFieldValue, errors, handleSubmit, values}) => (
					<>
						<FormControl variant="standard" component="fieldset">
							<p data-testid={"error_geen_keuze"} className={styles.errorLabel}>{errors.antwoord}</p>
							<RadioGroup
								name="antwoord"
								onChange={event => setFieldValue("antwoord", event.target.value)}
								value={values.antwoord || ""}>
								<ul className={styles.radiobuttons}>
									<li>
										<FormControlLabel
											value={ColonHeeftAsaScoreBovenDrie.JA}
											data-testid={"radio_ja"}
											control={<Radio/>}
											label={getString(gezondheidProperties.antwoord.ja)}
											className={styles.radioOptieBeschrijving}
										/>
									</li>
									<li>
										<FormControlLabel
											value={ColonHeeftAsaScoreBovenDrie.NEE}
											data-testid={"radio_nee"}
											control={<Radio/>}
											label={getString(gezondheidProperties.antwoord.nee)}
											className={styles.radioOptieBeschrijving}
										/>
									</li>
									<li>
										<FormControlLabel
											value={ColonHeeftAsaScoreBovenDrie.WEET_IK_NIET}
											data-testid={"radio_weet_ik_niet"}
											control={<Radio/>}
											label={getString(gezondheidProperties.antwoord.weet_ik_niet)}
											className={styles.radioOptieBeschrijving}
										/>
									</li>
								</ul>
							</RadioGroup>
							<SpanWithHtml value={getString(gezondheidProperties.niet_bewaard)}/>
						</FormControl>
						<div className={classNames(styles.bevestigenForm, styles.metVorige)}>
							<Button lightStyle={true} displayArrow={ArrowType.ARROW_LEFT}
							        onClick={() => navigate("/colon/afspraak/intake/")}
							        label={properties.afspraak_maken.button.vorige}/>
							<div className={styles.knoppenRechts}>
								<Button label={getString(properties.afspraak_maken.button.volgende)}
								        displayArrow={ArrowType.ARROW_RIGHT}

								        onClick={() => handleSubmit()}/>
								<NavLink onClick={openAfsluitenPopup} className={styles.andereOptie}>
									{getString(properties.afspraak_maken.button.afsluiten)}</NavLink>
							</div>
						</div>
					</>
				)}
			</Formik>
		</div>
	)
}

const WrappedColonAfspraakGezondheidPage: FC = () => (
	<ColonAfspraakWizard>
		<ColonAfspraakGezondheidPage/>
	</ColonAfspraakWizard>
)

export default WrappedColonAfspraakGezondheidPage
