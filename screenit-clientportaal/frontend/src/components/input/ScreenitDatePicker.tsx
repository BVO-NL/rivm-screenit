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
import {DatePicker, DateValidationError, LocalizationProvider} from "@mui/x-date-pickers"
import styles from "./ScreenitDatePicker.module.scss"
import {FormikErrors} from "formik"
import {getBovengrensUitLijst, getOndergrensUitLijst, isWerkdag, lijstBevatMeegegevenDatum} from "../../utils/DateUtil"
import {AdapterDateFns} from "@mui/x-date-pickers/AdapterDateFnsV3"
import {nl} from "date-fns/locale"
import {PickerChangeHandlerContext} from "@mui/x-date-pickers/models"

export type ScreenitDatePickerProps = {
	className?: string,
	propertyName: string,
	title?: string,
	label: string,
	errorLabel?: string | FormikErrors<any>,
	value?: Date | null,
	beschikbareDagen?: Date[],
	alleenWerkdagen?: boolean,
	onChange: (value: Date | null, context: PickerChangeHandlerContext<DateValidationError>) => void
}

const ScreenitDatePicker = (props: ScreenitDatePickerProps) => {

	const ondergrens = getOndergrensUitLijst(props.beschikbareDagen)
	const bovengrens = getBovengrensUitLijst(props.beschikbareDagen)

	return (
		<div>
			<p>{props.title}</p>
			<div className={props.errorLabel ? styles.inputDivError : styles.inputDiv}>
				<LocalizationProvider adapterLocale={nl} dateAdapter={AdapterDateFns}>
					<DatePicker
						views={["day"]}
						className={props.className}
						format="dd-MM-yyyy"
						label={props.label}
						value={props.value}
						onChange={props.onChange}
						shouldDisableDate={(date) => shouldDisableDate(date, ondergrens, bovengrens, props.beschikbareDagen, props.alleenWerkdagen)}
						slotProps={{textField: {variant: "standard", inputProps: {"data-testid": `input_${props.propertyName}`}}}}
					/>

				</LocalizationProvider>
				<p data-testid={`error_${props.propertyName}`} className={styles.errorLabel}>{props.errorLabel && String(props.errorLabel)}</p>
			</div>
		</div>
	)
}

export function shouldDisableDate(date: Date | null, ondergrens?: Date, bovengrens?: Date, beschikbareDagen?: Date[], alleenWerkdagen?: boolean) {
	if (date !== null && alleenWerkdagen && !isWerkdag(date)) {
		return true
	}
	if (beschikbareDagen && beschikbareDagen.length > 0) {
		if (date !== null && ondergrens && bovengrens && date >= ondergrens && date <= bovengrens) {
			return !lijstBevatMeegegevenDatum(beschikbareDagen, date)
		} else {
			return true
		}
	} else {
		return false
	}
}

export default ScreenitDatePicker
