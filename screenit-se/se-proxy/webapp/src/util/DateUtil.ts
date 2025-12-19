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
import {createActionKiesDaglijstDatum} from "../actions/DaglijstDatumActions"
import {store} from "../Store"
import {leesAfspraken} from "../restclient/DaglijstRestclient"
import {leesPlanning} from "../restclient/PlanningRestClient"
import {createActionNavigateToDaglijst} from "../actions/NavigationActions"
import {add, differenceInMinutes, Duration, format, formatDistanceToNow, formatDuration, isAfter, isBefore, isEqual, sub} from "date-fns"

export const DATUM_FORMAT = "yyyy-MM-dd"
export const NL_DATUM_FORMAT = "dd-MM-yyyy"
export const TIJD_FORMAT = "HH:mm"
export const TIMESTAMP_FORMAT = "HH:mm:ss.SSS"
export const ISO_TIMESTAMP_FORMAT = "yyyy-MM-dd'T'HH:mm:ss"
export const NL_DATUM_TIJD_FORMAT = "dd-MM-yyyy HH:mm"

export const vandaagISO = (): string => format(nu(), DATUM_FORMAT)
let offset: Duration | null = null

export const setOffset = (val: Duration): void => {
	offset = val

	if (store.getState().session) {
		store.dispatch(createActionKiesDaglijstDatum(vandaagISO()))
		leesAfspraken(vandaagISO(), createActionNavigateToDaglijst())
		leesPlanning(vandaagISO())
	}
}

export const nu = (): Date => {
	if (offset !== null) {
		return add(new Date(), offset)
	}

	return new Date()
}

export const nuISO = (): string => format(nu(), ISO_TIMESTAMP_FORMAT)

export const nuTimestamp = (): string => format(nu(), TIMESTAMP_FORMAT)

export const nuTijdUrenMinuten = (): string => format(nu(), TIJD_FORMAT)

export const ligtTussenData = (
	datum: Date,
	startDatum: Date | null,
	eindDatum: Date | null,
): boolean => {
	if (startDatum && eindDatum) {
		return (
			(isAfter(datum, startDatum) || isEqual(datum, startDatum)) &&
			(isBefore(datum, eindDatum) || isEqual(datum, eindDatum))
		)
	}
	if (startDatum) {
		return isAfter(datum, startDatum) || isEqual(datum, startDatum)
	}
	if (eindDatum) {
		return isBefore(datum, eindDatum) || isEqual(datum, eindDatum)
	}
	return false
}

export const datumFormaat = (isoDatum: string | Date | null | undefined): string => isoDatum ? format(new Date(String(isoDatum)), NL_DATUM_FORMAT) : ""

export const tijdFormaat = (isoTijd: string | undefined): string => isoTijd ? format(new Date(isoTijd), TIJD_FORMAT) : ""

export const datumTijdFormaat = (isoTijd: string | undefined): string => isoTijd ? format(new Date(isoTijd), NL_DATUM_TIJD_FORMAT) : ""

export const getDate = (isoDatetime: string): string => {
	return isoDatetime.split("T")[0]
}

export const getTime = (isoDatetime: string): string => {
	return isoDatetime.split("T")[1].slice(0, 5)
}

export const datumInVerleden = (isoDateTime: string): boolean => {
	const datum = new Date(isoDateTime)
	const gisteren = sub(nu(), {days: 1})
	return datum <= gisteren
}

export const datumInToekomst = (isoDateTime: string): boolean => {
	const datum = new Date(isoDateTime)
	const vandaagIso = nu()
	return datum > vandaagIso
}

export const getTijdGeledenTekst = (timestamp: string): string => {
	return formatDistanceToNow(new Date(timestamp), {addSuffix: true})
}

export const vandaagPlusDagen = (aantal: number): Date => {
	return add(nu(), {days: aantal})
}

export const vandaagDate = (): Date => {
	return new Date(vandaagISO())
}

export const isValideDatum = (datum: Date): boolean => {
	return datum.getFullYear().toString().length === 4
}

export const stringDatumEnStringTijdNaarDatumTijd = (datum: string, tijd: string): Date => {
	return new Date(`${datum}T${tijd}`)
}

export const verschilDatumTijdEnNuInMinuten = (datumTijd: string | Date): number => {
	return differenceInMinutes(new Date(nuISO()), new Date(datumTijd))
}

export const numberMinutenNaarStringUrenEnMinuten = (minuten: number): string => {
	return formatDuration({minutes: minuten})
}

export const javaDurationToJavascript = (durationStr: string): Duration => {
	const match = durationStr.match(
		/^P(?:(-?\d+)W)?(?:(-?\d+)D)?(?:T(?:(-?\d+)H)?(?:(-?\d+)M)?(?:(-?\d+(?:\.\d+)?)S)?)?$/,
	)
	if (!match) {
		throw new Error(`Invalid Java Duration format: ${durationStr}`)
	}
	const [, w, d, h, m, s] = match
	const days = (w ? parseInt(w, 10) * 7 : 0) + (d ? parseInt(d, 10) : 0)
	return {
		days,
		hours: h ? parseInt(h, 10) : 0,
		minutes: m ? parseInt(m, 10) : 0,
		seconds: s ? parseFloat(s) : 0,
	}
}
