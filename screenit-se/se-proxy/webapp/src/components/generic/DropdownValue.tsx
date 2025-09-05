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
import React from "react"
import Select from "react-select"

export type DropdownValueProps<T> = {
	id: string;
	value?: T;
	disabled: boolean;
	options: Array<NonNullable<T>>;
	required?: boolean;
	isWhite?: boolean;
	placeholder?: string;
	lavendel?: boolean;
	valueToLabel?: (value: T) => string;
	handleChange: (value?: T) => void;
};

type DropdownOption<T> = {
	value: NonNullable<T>;
	label: string;
}

export default class DropdownValue<T> extends React.Component<DropdownValueProps<T>> {

	constructor(props: DropdownValueProps<T>) {
		super(props)
		this.handleChange = this.handleChange.bind(this)
	}

	handleChange = (selectedOption: DropdownOption<T> | null): void => {
		this.props.handleChange(selectedOption && selectedOption.value ? selectedOption.value : undefined)
	}

	render(): JSX.Element {
		return <div
			className={this.props.lavendel ? "select-container-lavendel" : ""}
			title={this.props.value ? (this.props.valueToLabel ? this.props.valueToLabel(this.props.value) : String(this.props.value)) : ""}>
			<Select id={this.props.id}
					styles={{
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
							backgroundColor: this.props.isWhite ? "#fff" : "lavender",
						}),
						option: (provided): any => ({
							...provided,
							":hover": !this.props.isWhite && {
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
							backgroundColor: this.props.isWhite ? "#fff" : "lavender",
							color: "#000!important",
						}),
					}} placeholder={this.props.placeholder ? this.props.placeholder : "Kies..."} value={this.props.value ? {
				value: this.props.value,
				label: this.props.valueToLabel ? this.props.valueToLabel(this.props.value) : String(this.props.value),
			} : null} onChange={this.handleChange} options={this.props.options.map(v => {
				return {
					value: v,
					label: this.props.valueToLabel ? this.props.valueToLabel(v) : String(v),
				}
			})} isDisabled={this.props.disabled} isClearable={!this.props.required} noOptionsMessage={(): string => "Geen resultaten"}/>
		</div>
	}

}
