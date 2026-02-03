/*-
 * ========================LICENSE_START=================================
 * screenit-web
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
var imsContextBridgeUrl = "https://localhost:7001/" 
var allImagesSeenRequestBody 
var imsErrorCallback 
let currentImsMessage
let queuedImsMessage

function sendOrQueueImsMessage(messageType, usersession, onderzoekIdVoorErrorCallback, customCallback) {
	const imsMessage = {usersession, customCallback, messageType, onderzoekIdVoorErrorCallback}
	if (!currentImsMessage) {
		currentImsMessage = imsMessage
		sendCurrentImsMessage()
	} else if (messageType === "EmptyDesktopSync") {
		abortPendingRequest()
		currentImsMessage = imsMessage
		sendCurrentImsMessage()
	} else {
		console.log("IMS: Queue usersession %s. Existing usersession in queue overwritten: %s", messageType, !!queuedImsMessage)
		queuedImsMessage = imsMessage
	}
}

function abortPendingRequest() {
	if (currentImsMessage && currentImsMessage.abortController) {
		console.log("IMS: Abort active request of type: %s, message in queue: %s ", currentImsMessage.messageType, !!queuedImsMessage)
		queuedImsMessage = null
		currentImsMessage.abortController.abort()
	}
}

async function sendCurrentImsMessage() {
	currentImsMessage.abortController = new AbortController()
	const timeoutId = setTimeout(() => currentImsMessage.abortController.abort(), 30000)
	const options = {
		method: "PUT",
		headers: {
			"Content-type": "application/json; charset=utf-8"
		},
		body: JSON.stringify(currentImsMessage.usersession),
		signal: currentImsMessage.abortController.signal
	}
	console.log("IMS: Start sending Usersession to MammoBridge type:", currentImsMessage.messageType)
	if (currentImsMessage.messageType === "ClientDesktopSync") {
		console.timeEnd("IMS: Afronden click till start sending desktopsync")
	}
	let data
	try {
		const response = await fetch(imsContextBridgeUrl, options)
		clearTimeout(timeoutId)
		if (response.ok) {
			data = await response.json()
			console.log("IMS: HttpOk response received")
			checkResponseSync(data)
			if (currentImsMessage.customCallback) {
				currentImsMessage.customCallback(JSON.stringify(data))
			}
		} else {
			console.error("IMS: MammoBridge error (see netwerklog)")
			reportError("error")
		}
	}
	catch (error) {
		clearTimeout(timeoutId)
		if (error.name === "AbortError") {
			console.error("IMS: MammoBridge timout")
			reportError("timeout")
		} else {
			console.error("IMS: MammoBridge error")
			reportError("error")
		}
	}
	finally {
		console.log(
			"IMS: Call to ImsBridge %s finished. Pending message: %s",
			currentImsMessage.messageType,
			!!queuedImsMessage
		)
		currentImsMessage = queuedImsMessage
		queuedImsMessage = null
		if (currentImsMessage) {
			sendCurrentImsMessage()
		}
	}
}

function sendUserSessionToImsBridge(messageType, usersession, onderzoekIdVoorErrorCallback) { 
	sendOrQueueImsMessage(messageType, usersession, onderzoekIdVoorErrorCallback, null)
}

function sendAllImagesSeenRequest(messageType, onderzoekIdVoorErrorCallback, allImagesSeenCallback) { 
	const customCallback = function (responseTekst) {
		allImagesSeenCallback(responseTekst)
	}
	sendOrQueueImsMessage(messageType, allImagesSeenRequestBody, onderzoekIdVoorErrorCallback, customCallback)
}

function checkResponseSync(response) {
	console.log("IMS response: ", response.context)
	if (response.context.syncStatus !== undefined) {
		if (response.context.syncStatus.syncCode.identifier.value === "ERROR-SYNC") {
			reportError("outofsync")
		}
	} else if (response.context.layoutImages !== undefined) {
		if (response.context.layoutImages.requestLayoutsImagesSeenCurrentFocus.reply === "Patient ID in PACS != Patient ID in ScreenIT") {
			reportError("outofsync")
		}
	}
}

function reportError(errorMessage) {
	imsErrorCallback(errorMessage, currentImsMessage.onderzoekIdVoorErrorCallback)
}
