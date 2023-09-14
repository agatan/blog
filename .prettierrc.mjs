export default {
  "plugins": ["prettier-plugin-astro", "prettier-plugin-tailwindcss"],
  "arrowParens": "avoid",
  "semi": true,
  "tabWidth": 2,
  "printWidth": 80,
  "singleQuote": false,
  "jsxSingleQuote": false,
  "trailingComma": "es5",
  "bracketSpacing": true,
  "endOfLine": "lf",
  "overrides": [
    {
      "files": "src/**/*.astro",
      "options": {
        "parser": "astro"
      }
    }
  ]
}
