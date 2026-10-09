import {createViteConfig} from "../../vite.config.base.mts"
import {resolve} from "path"

export default createViteConfig({
	enableSvgr: true,
	additionalResolveAlias: {
		"@": resolve(import.meta.dirname, "src"),
	},
	scssAdditionalData: `
		@import "@/scss/media-mixins.scss";
		@import "@/scss/mixins.scss";
	`,
})
