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
import {ChangeEvent, Component, JSX} from "react"
import {Input} from "reactstrap"

export type TextValueProps = {
	value: string;
	maxLength?: number;
	className?: string;
	placeholder?: string;
	disabled: boolean;
	color?: string;
	onChange: (value: string) => void;
};

type TextValueState = {
	value: string;
};

export default class TextAreaValue extends Component<TextValueProps, TextValueState> {

	constructor(props: TextValueProps) {
		super(props)
		this.state = {
			value: props.value,
		}
		this.updateValue = this.updateValue.bind(this)
	}

	componentDidUpdate(prevProps: TextValueProps): void {
		if (this.props.value !== prevProps.value && this.props.value !== this.state.value) {
			this.setState({
				value: this.props.value,
			})
		}
	}

	updateValue(event: ChangeEvent<HTMLInputElement>): void {
		const target = event.target

		if (target instanceof HTMLTextAreaElement) {
			this.setState({
				value: target.value,
			})
			this.props.onChange(target.value)
		}
	}

	render(): JSX.Element {
		return <div className={this.props.className}>
			<Input type={"textarea"} disabled={this.props.disabled} value={this.state.value}
				   placeholder={this.props.placeholder} onChange={this.updateValue}
				   maxLength={this.props.maxLength} style={{
				backgroundColor: this.props.color,
			}}/>
		</div>
	}

}
