import {defineConfig, type UserConfigExport} from "vite"
import react from "@vitejs/plugin-react"
import browserslistToEsbuild from "browserslist-to-esbuild"
import svgr from "vite-plugin-svgr"

interface ViteConfigOptions {
	port?: number
	proxyTarget?: string
	additionalPlugins?: any[]
	additionalResolveAlias?: Record<string, string>
	cssModulesNameGenerator?: (name: string, filename: string) => string
	scssAdditionalData?: string
	enableSvgr?: boolean
}

export const createViteConfig = (options: ViteConfigOptions = {}): UserConfigExport => {
	const {
		port = 3000,
		proxyTarget = "http:
		additionalPlugins = [],
		additionalResolveAlias = {},
		cssModulesNameGenerator,
		scssAdditionalData = "",
		enableSvgr = false,
	} = options

	const plugins: any[] = [react(), ...additionalPlugins]

	if (enableSvgr) {
		plugins.push(
			svgr({
				svgrOptions: {exportType: "default", ref: true, svgo: false, titleProp: true},
				include: "**/*.svg",
			}),
		)
	}

	return defineConfig({
		build: {
			target: browserslistToEsbuild(),
		},
		plugins,
		server: {
			port,
			host: true,
			allowedHosts: ["host.docker.internal"],
			proxy: {
				"/api": {
					target: proxyTarget,
					changeOrigin: true,
				},
			},
		},
		css: {
			preprocessorOptions: {
				scss: {
					api: "modern-compiler",
					silenceDeprecations: ["color-functions", "global-builtin", "import"],
					...(scssAdditionalData && {additionalData: scssAdditionalData}),
				},
			},
			modules: {
				generateScopedName:
					cssModulesNameGenerator ||
					((name, filename) => {
						const basename = filename.split("/").pop()?.replace(".module.scss", "").replace(".scss", "") || "unknown"
						let hash = 0
						for (let i = 0; i < filename.length; i++) {
							const char = filename.charCodeAt(i)
							hash = ((hash << 5) - hash) + char
							hash = hash & hash
						}
						const hashStr = Math.abs(hash).toString(36).slice(0, 5)
						return `${basename}_${name}_${hashStr}`
					}),
			},
		},
		resolve: {
			alias: additionalResolveAlias,
		},
	})
}

export default createViteConfig()
