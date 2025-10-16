
const eslint = require('@eslint/js')
const tseslint = require('typescript-eslint')
const angular = require('angular-eslint')

module.exports = tseslint.config(
  {
    files: ['**/*.ts'],
    extends: [eslint.configs.recommended, ...tseslint.configs.recommended, ...tseslint.configs.stylistic, ...angular.configs.tsRecommended],
    processor: angular.processInlineTemplates,
    rules: {
      '@angular-eslint/directive-selector': [
        'error',
        {
          type: 'attribute',
          prefix: 'app',
          style: 'camelCase',
        },
      ],
      '@angular-eslint/component-selector': [
        'error',
        {
          type: 'element',
          prefix: 'app',
          style: 'kebab-case',
        },
      ],

      '@typescript-eslint/member-delimiter-style': [0],
      '@typescript-eslint/no-extra-semi': [0],
      '@typescript-eslint/no-explicit-any': [2],
      'no-nested-ternary': [2],
      '@typescript-eslint/no-unused-expressions': [
        2,
        {
          allowTernary: true,
        },
      ],
    },
  },
  {
    files: ['**/*.html'],
    extends: [...angular.configs.templateRecommended, ...angular.configs.templateAccessibility],
    rules: {},
  },
)
