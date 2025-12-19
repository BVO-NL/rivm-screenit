/*-
 * ========================LICENSE_START=================================
 * screenit-se-proxy
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
import * as React from "react"
import {JSX} from "react"
import DatePicker, {registerLocale} from "react-datepicker"
import "react-datepicker/dist/react-datepicker.css"
import {nl} from "date-fns/locale/nl"
import {DATUM_FORMAT, NL_DATUM_FORMAT, vandaagPlusDagen} from "../../util/DateUtil"
import {format} from "date-fns"

registerLocale("nl", nl)

export type DatumkiezerViewStateProps = {
	daglijstDatum: string;
	online: boolean;
	dagenDaglijstOphalenLimiet?: number;
};

export type DatumkiezerViewDispatchProps = {
	onChooseDate: (datum: string, online: boolean, limiet?: number) => void;
}

const DatumkiezerView = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): JSX.Element => {
	return <div className="row datumkiezer-div">
		<div className="col-10 row-no-gutters">
			{}
			<DatePicker
				className="datumkiezer-view clickable"
				locale="nl"
				value={getDatumkiezerValue(props)}
				highlightDates={getHighlightDates(props)}
				onChange={(newDate: Date | null): void => {
					if (newDate) {
						props.onChooseDate(format(newDate, DATUM_FORMAT), props.online, props.dagenDaglijstOphalenLimiet)
					}
				}}
				onKeyDown={(event: React.KeyboardEvent<HTMLElement>): void => event.preventDefault()}
			/>
		</div>
		<div className="col-2 datumkiezer-icon">
			<i className="fa fa-calendar px-1 py-1"/>
		</div>
	</div>
}

const getDatumkiezerValue = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): string => {
	return format(new Date(props.daglijstDatum), `cccc ${NL_DATUM_FORMAT}`)
}

const getHighlightDates = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): Date[] => {
	const dates: Date[] = []
	if (props.dagenDaglijstOphalenLimiet) {
		for (let i = 0; i <= props.dagenDaglijstOphalenLimiet; i++) {
			dates.push(vandaagPlusDagen(i))
		}
	}
	return dates
}

export default DatumkiezerView
