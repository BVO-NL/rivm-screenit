/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React, {Component} from "react"
import Select from "react-select"

export type MultiselectProps<T> = {
	id: string;
	options: MultiselectOption<T>[];
	disabled?: boolean;
	handleChange: (selectedOptions: T[]) => void;
	value?: MultiselectOption<T>[];
};

type MultiselectOption<T> = {
	value: T;
	label: string;
}

export default class MultiselectValue<T> extends Component<MultiselectProps<T>> {

	constructor(props: MultiselectProps<T>) {
		super(props)
		this.handleChange = this.handleChange.bind(this)
	}

	handleChange = (selectedOptions: readonly MultiselectOption<T>[]): void => {
		this.props.handleChange(selectedOptions ? selectedOptions.map(v => v.value) : [])
	}

	render(): JSX.Element {
		return <Select className={"select-container-lavendel multiselect-container"} id={this.props.id} styles={{
			placeholder: (provided): any => ({
				...provided,
				color: "black",
			}),
			dropdownIndicator: (provided): any => ({
				...provided,
				color: "black",
			}),
			clearIndicator: (provided): any => ({
				...provided,
				color: "black",
			}),
			menu: (provided): any => ({
				...provided,
				backgroundColor: "lavender",
			}),
			option: (provided): any => ({
				...provided,
				":hover": {
					backgroundColor: "#D8C1FB",
					color: "#54026e",
					cursor: "pointer",
				},
			}),
			container: (provided): any => ({
				...provided,
				width: "auto",
				color: "black",
			}),
			control: (provided): any => ({
				...provided,
				backgroundColor: "lavender",
				color: "#000!important",
			}),
			multiValue: (provided): any => ({
				...provided,
				backgroundColor: "#D8C1FB",
			}),
		}} placeholder={"Kies..."} value={this.props.value} onChange={this.handleChange} options={this.props.options}
					   isDisabled={this.props.disabled} isMulti={true} isClearable={false}
					   noOptionsMessage={(): string => "Geen resultaten"}/>
	}

}
