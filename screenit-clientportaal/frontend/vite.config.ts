import {createViteConfig} from "../../vite.config.base"
import {resolve} from "path"

export default createViteConfig({
	enableSvgr: true,
	additionalResolveAlias: {
		"@": resolve(__dirname, "src"),
	},
	scssAdditionalData: `
		@import "@/scss/media-mixins.scss";
		@import "@/scss/mixins.scss";
	`,
})
