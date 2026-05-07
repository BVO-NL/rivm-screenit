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

import * as hash from "hash.js"

async function sha256DigestFallback(algorithm: string, data: BufferSource): Promise<ArrayBuffer> {
	if (algorithm !== "SHA-256") {
		throw new Error(`Unsupported algorithm: ${algorithm}. Only SHA-256 is supported in the polyfill.`)
	}

	const uint8Array = data instanceof ArrayBuffer
		? new Uint8Array(data)
		: new Uint8Array(data.buffer, data.byteOffset, data.byteLength)

	const hashArray = hash.sha256().update(Array.from(uint8Array)).digest()

	return new Uint8Array(hashArray).buffer
}

function generateUUIDFallback(): string {
	return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, (c) => {
		const r = Math.random() * 16 | 0
		const v = c === "x" ? r : (r & 0x3 | 0x8)
		return v.toString(16)
	})
}

function getRandomValuesFallback<T extends ArrayBufferView>(array: T): T {
	const uint8Array = new Uint8Array(array.buffer, array.byteOffset, array.byteLength)
	for (let i = 0; i < uint8Array.length; i++) {
		uint8Array[i] = Math.floor(Math.random() * 256)
	}
	return array
}

export function installCryptoPolyfills(): void {
	const isSecureContext = window.isSecureContext || window.location.hostname === "localhost"

	if (!isSecureContext) {
		console.warn(
			"[Crypto Polyfill] Running in non-secure context. Installing fallback crypto functions. " +
			"Deze zijn NIET cryptografisch veilig en mogen ALLEEN in test omgevingen worden gebruikt!",
		)

		if (!window.crypto) {
			Object.defineProperty(window, "crypto", {value: {}, configurable: true})
		}

		if (!window.crypto.randomUUID) {
			window.crypto.randomUUID = generateUUIDFallback
		}

		if (!window.crypto.getRandomValues) {
			window.crypto.getRandomValues = getRandomValuesFallback
		}

		if (!window.crypto.subtle) {
			Object.defineProperty(window.crypto, "subtle", {value: {}, configurable: true})
		}

		if (!window.crypto.subtle.digest) {
			window.crypto.subtle.digest = sha256DigestFallback
		}
	}
}
