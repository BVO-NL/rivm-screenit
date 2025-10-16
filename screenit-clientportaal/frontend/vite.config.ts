import {defineConfig} from "vite"
import react from "@vitejs/plugin-react"
import {resolve} from "path"
import browserslistToEsbuild from "browserslist-to-esbuild"
import svgr from "vite-plugin-svgr"
import readableClassnames from "vite-plugin-readable-classnames"

export default defineConfig({
	build: {
		target: browserslistToEsbuild(),
	},
	plugins: [
		react(),
		svgr({
			svgrOptions: {exportType: "default", ref: true, svgo: false, titleProp: true},
			include: "**/*.svg",
		}),
		readableClassnames({separator: {beforeClassName: "_"}}),
	],
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
				additionalData: `
					@import "@/scss/media-mixins.scss";
					@import "@/scss/mixins.scss";
				`,
			},
		},
	},
	resolve: {
		alias: {
			"@": resolve(__dirname, "src"),
		},
	},
})
