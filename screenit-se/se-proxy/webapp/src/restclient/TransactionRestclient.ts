/*-
 * ========================LICENSE_START=================================
 * se-proxy
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
import {fetchApiPromise} from "../util/ApiUtil"
import type {Transaction, TransactionType} from "../datatypes/Transaction"
import {store} from "../Store"
import type {SeAction} from "../actions/SeAction"
import {Afspraak} from "../datatypes/Afspraak"
import {nuTimestamp} from "../util/DateUtil"

export const putTransactionToScreenItCentraalPromiseZonderAfspraak = (clientId: number, uitnodigingsNr: number | undefined, type: TransactionType, ...actions: Array<SeAction>): Promise<any> => {
	const session = store.getState().session
	if (!session) {
		console.log(`${nuTimestamp()} geen sessie gevonden bij plaatsen transactie`)
	}
	const transaction: Transaction = {
		type: type,
		clientId: clientId,
		medewerkercode: session?.medewerkercode,
		instellingGebruikerId: session?.instellingGebruikerId,
		afspraakVanafDatum: store.getState().daglijstDatum,
		uitnodigingsNr: uitnodigingsNr,
		actions: actions,
	}
	return promisePutTransactionToCentraal(transaction)
}

const createTransactionWithActionArray = (afspraak: Afspraak | undefined, type: TransactionType, actions: Array<SeAction>): Transaction => {
	const session = store.getState().session
	return {
		type: type,
		clientId: afspraak && afspraak.clientId,
		medewerkercode: session?.medewerkercode,
		instellingGebruikerId: session?.instellingGebruikerId,
		afspraakVanafDatum: afspraak ? afspraak.vanafDatum : store.getState().daglijstDatum,
		uitnodigingsNr: afspraak && afspraak.uitnodigingsNr,
		actions: actions,
	}
}

export const createTransaction = (afspraak: Afspraak | undefined, type: TransactionType, ...actions: Array<SeAction>): Transaction => {
	return createTransactionWithActionArray(afspraak, type, actions)
}

export const putTransactionsToScreenItCentraalPromise = (transactions: Array<Transaction>): Promise<any> => {
	return promisePutTransactionToCentraal(transactions)
}

export const putTransactionToScreenItCentraalPromise = (afspraak: Afspraak | undefined, type: TransactionType, ...actions: Array<SeAction>): Promise<any> => {
	const transaction = createTransactionWithActionArray(afspraak, type, actions)
	return promisePutTransactionToCentraal(transaction)
}

export const promisePutTransactionToCentraal = (transaction: Transaction | Array<Transaction>): Promise<any> => {
	return new Promise((resolve) => {
		transactionQueue.unshift({
			transaction: transaction,
			transactionResolve: resolve,
		})

		if (!verwerkingInProgress) {
			verwerkQueue()
		}
	})
}
let verwerkingInProgress = false

type TransactionQueueItem = {
	transaction: Transaction | Array<Transaction>,
	transactionResolve: (value?: any) => void;
}
const transactionQueue: TransactionQueueItem[] = []

const verwerkQueue = (): void => {
	verwerkingInProgress = true

	if (transactionQueue.length === 0) {
		verwerkingInProgress = false
		return
	}

	const transactionObject = transactionQueue.pop()
	if (transactionObject) {
		fetchApiPromise("PUT", "putTransactionToScreenItCentraal", JSON.stringify(transactionObject.transaction)).then(() => {
			transactionObject.transactionResolve()
			verwerkQueue()
		})
	}
}
