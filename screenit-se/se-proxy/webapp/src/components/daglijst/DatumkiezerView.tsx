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
import moment from "moment"
import "react-datepicker/dist/react-datepicker.css"
import {nl} from "date-fns/locale/nl"
import {vandaagPlusDagen} from "../../util/DatePickerUtil"

registerLocale("nl", nl)
moment.updateLocale("nl", {
	months: ["januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus", "september", "oktober", "november", "december"],
	weekdaysMin: ["zo", "ma", "di", "wo", "do", "vr", "za"],
	week: {
		dow: 1,
	},
})

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
						props.onChooseDate(moment(newDate).format("YYYY-MM-DD"), props.online, props.dagenDaglijstOphalenLimiet)
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
	moment.locale("nl")
	const datum = moment(props.daglijstDatum)
	return datum.format("dddd DD-MM-YYYY")
}

const getHighlightDates = (props: DatumkiezerViewStateProps & DatumkiezerViewDispatchProps): Date[] => {
	const dates: Date[] = []
	if (props.dagenDaglijstOphalenLimiet) {
		for (let i = 0; i <= props.dagenDaglijstOphalenLimiet; i++) {
			dates.push(moment(vandaagPlusDagen(i)).toDate())
		}
	}
	return dates
}

export default DatumkiezerView
