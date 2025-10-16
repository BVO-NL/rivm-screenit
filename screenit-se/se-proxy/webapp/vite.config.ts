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
import {defineConfig} from "vite"
import react from "@vitejs/plugin-react"
import browserslistToEsbuild from "browserslist-to-esbuild"
import readableClassnames from "vite-plugin-readable-classnames"

export default defineConfig({
	build: {
		target: browserslistToEsbuild(),
	},
	plugins: [react(), readableClassnames({separator: {beforeClassName: "_"}})],
	server: {
		port: 3010,
		host: true,
		allowedHosts: ["host.docker.internal"],
		proxy: {
			"/api": {
				target: "http:
				changeOrigin: true,
			},
		},
	},
	css: {
		preprocessorOptions: {
			scss: {
				api: "modern-compiler",
				silenceDeprecations: ["color-functions", "global-builtin", "import"],
			},
		},
	},
})
