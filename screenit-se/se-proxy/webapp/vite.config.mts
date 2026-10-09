import {createViteConfig} from "../../../vite.config.base.mts"

export default createViteConfig({
	port: 3010,
	proxyTarget: "http://localhost:8090",
})
