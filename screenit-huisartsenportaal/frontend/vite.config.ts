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
		port: 3000,
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
