import {createViteConfig} from "../../vite.config.base.mts"
import {resolve} from "path"
import {loadEnv, type Plugin} from "vite"

const MOPINION_DEPLOYMENT_ID_PLACEHOLDER = "theMopinionDeploymentId"

// In de Docker container vervangt nginx de placeholder in index.html (zie nginx/nginx.conf.template), voor de Vite dev server doet deze plugin dat
const mopinionDeploymentIdPlugin = (): Plugin => {
	let deploymentId = ""
	return {
		name: "mopinion-deployment-id",
		apply: "serve",
		configResolved(config) {
			deploymentId = loadEnv(config.mode, config.root, "").MOPINION_DEPLOYMENT_ID ?? ""
		},
		transformIndexHtml: html => html.replace(`content="${MOPINION_DEPLOYMENT_ID_PLACEHOLDER}"`, `content="${deploymentId}"`),
	}
}

export default createViteConfig({
	enableSvgr: true,
	additionalPlugins: [mopinionDeploymentIdPlugin()],
	additionalResolveAlias: {
		"@": resolve(import.meta.dirname, "src"),
	},
	scssAdditionalData: `
		@import "@/scss/media-mixins.scss";
		@import "@/scss/mixins.scss";
	`,
})
