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
declare global {
	interface Window {
		Pastease?: {
			load: (id: string) => void
		}
	}
}

class MopinionService {
	private readonly PASTEASE_POLL_INTERVAL_MS = 50
	private readonly DEPLOYMENT_ID_PLACEHOLDER = "theMopinionDeploymentId"

	init(): void {
		const deploymentId = this.bepaalDeploymentId()
		if (!deploymentId) {
			return
		}
		const interval = setInterval(() => {
			if (window.Pastease) {
				clearInterval(interval)
				window.Pastease.load(deploymentId)
			}
		}, this.PASTEASE_POLL_INTERVAL_MS)
	}

	private bepaalDeploymentId(): string | undefined {
		const deploymentId = document.querySelector<HTMLMetaElement>("meta[property=\"mopinion-deployment-id\"]")?.content
		if (!deploymentId || deploymentId === this.DEPLOYMENT_ID_PLACEHOLDER) {
			return undefined
		}
		return deploymentId
	}
}

const mopinionService: MopinionService = new MopinionService()
export default mopinionService
