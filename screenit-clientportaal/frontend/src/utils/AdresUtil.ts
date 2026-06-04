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
import {Adres} from "../datatypes/adres/Adres"
import {isNullOfLeeg} from "./EmptyUtil"

export const getVolledigeAdresString = (adres?: Adres): string => {
	if (!adres) {
		return ""
	}
	let adresString = getAdres(adres)
	const postcode = adres.postcode
	const plaats = adres.plaats
	if (adresString && (postcode || plaats)) {
		adresString += ","
	}
	if (postcode) {
		if (adresString) {
			adresString += " "
		}
		adresString += formatPostcode(postcode)
	}
	if (plaats) {
		if (adresString) {
			adresString += " "
		}
		adresString += plaats
	}
	return adresString
}

const formatPostcode = (postcode: string): string => {
	if (!postcode) {
		return ""
	}
	const cleaned = postcode.replace(/\s+/g, "")
	if (cleaned.length === 6) {
		return cleaned.substring(0, 4) + " " + cleaned.substring(4).toUpperCase()
	}
	return postcode
}

export const getHuisnummerVolledig = (adres: Adres): string => {
	let adresString = ""
	if (adres.huisnummer !== undefined && adres.huisnummer !== null) {
		adresString += adres.huisnummer
	}
	if (adres.huisletter) {
		if (adresString) {
			adresString += " "
		}
		adresString += adres.huisletter
	}
	if (adres.huisnummerToevoeging) {
		if (adresString) {
			adresString += " "
		}
		adresString += adres.huisnummerToevoeging
	}
	if (adres.huisnummerAanduiding) {
		if (adresString) {
			adresString += " "
		}
		adresString += adres.huisnummerAanduiding
	}
	return adresString
}

export const getAdres = (adres: Adres): string => {
	if (!adres) {
		return ""
	}
	if (!isNullOfLeeg(adres.straat)) {
		let s = adres.straat!
		const huisnummerVolledig = getHuisnummerVolledig(adres)
		if (huisnummerVolledig) {
			s += " " + huisnummerVolledig
		}
		return s
	} else if (!isNullOfLeeg(adres.locatieBeschrijving)) {
		return adres.locatieBeschrijving!
	}
	return ""
}

export const isOnvolledigAdres = (adres?: Adres): boolean => {
	if (!adres) {
		return true
	}
	return isNullOfLeeg(adres.postcode) || adres.huisnummer === undefined || adres.huisnummer === null ||
		!isNullOfLeeg(adres.locatieBeschrijving) || isNullOfLeeg(adres.plaats)
}

export const bepaalMissendeAdresgegevensString = (adres: Adres): string => {
	const missendeAdresgegevens: string[] = []
	if (!straatHuisnummerVolledig(adres)) {
		if (isNullOfLeeg(adres.straat)) {
			missendeAdresgegevens.push("straat")
		}
		if (adres.huisnummer === undefined || adres.huisnummer === null) {
			missendeAdresgegevens.push("huisnummer")
		}
	} else if (!locatiebeschrijvingGevuld(adres)) {
		missendeAdresgegevens.push("locatie beschrijving")
	}
	if (isNullOfLeeg(adres.plaats)) {
		missendeAdresgegevens.push("plaatsnaam")
	}
	if (isNullOfLeeg(adres.postcode)) {
		missendeAdresgegevens.push("postcode")
	}
	return missendeAdresgegevens.join(", ")
}

export const createKixCode = (adres: Adres): string => {
	if (!adres || isNullOfLeeg(adres.postcode)) {
		return ""
	}
	let kixcode = adres.postcode!.replace(/\s+/g, "").replace(/[^A-Za-z0-9]/g, "").toUpperCase()
	if (adres.huisnummer !== undefined && adres.huisnummer !== null) {
		kixcode += adres.huisnummer
	}
	if (adres.huisnummerToevoeging) {
		kixcode += "X" + adres.huisnummerToevoeging.replace(/[^A-Za-z0-9]/g, "").toUpperCase()
	}
	if (adres.huisnummerAanduiding) {
		kixcode += "X" + adres.huisnummerAanduiding.replace(/[^A-Za-z0-9]/g, "").toUpperCase()
	}
	if (adres.huisletter) {
		kixcode += "X" + adres.huisletter.toUpperCase()
	}
	return kixcode
}

const straatHuisnummerVolledig = (adres: Adres): boolean => {
	return !isNullOfLeeg(adres.straat) && adres.huisnummer !== undefined && adres.huisnummer !== null
}
const locatiebeschrijvingGevuld = (adres: Adres): boolean => {
	return !isNullOfLeeg(adres.locatieBeschrijving)
}
export const getAdresStringMetHtmlSeparator = (adres: string): string => {
	return adres.replaceAll(",", "<br>");
}
