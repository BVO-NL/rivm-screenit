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
import {Button, Modal, ModalBody, ModalFooter, ModalHeader} from "reactstrap"
import AlleenOnlineButton from "../AlleenOnlineButton"

export type AkkoordPopupViewStateProps = {
	visible: boolean;
	titel?: string;
	body?: React.ReactNode;
	callback?: (...args: Array<any>) => any;
	cancelCallback?: ((...args: Array<any>) => any);
	akkoordString?: string;
	annulerenString?: string;
	online?: boolean;
	alleenOnline?: boolean;
};

export type AkkoordPopupViewDispatchProps = {
	akkoord: (callback?: (...args: Array<any>) => any) => void;
	cancel: (cancelCallback?: ((...args: Array<any>) => any)) => void;
}

export default class AkkoordPopupView extends React.Component<AkkoordPopupViewStateProps & AkkoordPopupViewDispatchProps> {

	render(): JSX.Element {
		return <div>
			<Modal isOpen={this.props.visible} toggle={(): void => {
				this.props.cancel(this.props.cancelCallback)
			}} className={""}>
				<ModalHeader toggle={(): void => {
					this.props.cancel(this.props.cancelCallback)
				}}>{this.props.titel}</ModalHeader>
				<ModalBody className={"pb-0"}>
					{this.props.body}
				</ModalBody>

				{(this.props.akkoordString || this.props.annulerenString) && <ModalFooter>
					{this.props.akkoordString && (this.props.alleenOnline ?
						<AlleenOnlineButton id={this.props.akkoordString.replaceAll(" ", "-")} color="primary" label={this.props.akkoordString}
											online={this.props.online ?? false}
											onClick={(): void => {
												this.props.akkoord(this.props.callback)
											}}/> :
						<Button color="primary" onClick={(): void => {
							this.props.akkoord(this.props.callback)
						}}>{this.props.akkoordString}</Button>)}
					{this.props.annulerenString && <Button color="secondary" onClick={(): void => {
						this.props.cancel(this.props.cancelCallback)
					}}>{this.props.annulerenString}</Button>}
					{!this.props.online && this.props.alleenOnline && <img className={"geen-verbinding-modal"} src="images/geen-verbinding.png" alt="geen-verbinding"/>}
				</ModalFooter>}
			</Modal>
		</div>
	}

}
