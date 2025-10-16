import {defineConfig} from "eslint/config"
import reactPlugin from "eslint-plugin-react"
import tseslint from "typescript-eslint"
import globals from "globals"

export default defineConfig([
	reactPlugin.configs.flat.recommended,
	reactPlugin.configs.flat["jsx-runtime"],
	tseslint.configs.recommended,
	{
		"settings": {
			"react": {
				"version": "detect"
			}
		},
		files: ["**/*.{js,mjs,cjs,jsx,mjsx,ts,tsx,mtsx}"],
		languageOptions: {
			globals: globals.browser
		},
		"rules": {
			"quotes": [
				"error",
				"double"
			],
			"semi": [
				"error",
				"never"
			],
			"@typescript-eslint/no-unused-vars": "error",
			"@typescript-eslint/ban-ts-comment": "off",
			"@typescript-eslint/no-explicit-any": "off",
			"@typescript-eslint/explicit-function-return-type": [
				"error",
				{
					"allowTypedFunctionExpressions": true
				}
			],
			"@typescript-eslint/explicit-module-boundary-types": "off",
			"no-unused-vars": "off",
			"no-prototype-builtins": "error",
			"no-array-constructor": "error",
			"prefer-template": "error",
			"prefer-rest-params": "error",
			"prefer-arrow-callback": "error",
			"no-dupe-class-members": "error",
			"no-useless-constructor": "error",
			"dot-notation": "error",
			"eqeqeq": "error"
		}
	}])
