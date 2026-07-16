const fs = require('fs')

fs.cp('node_modules/@topicus-rgp-ds/web/assets/health-icons', '../../screenit-web/src/main/resources/static/assets/health-icons', { recursive: true }, (err) => {
  if (err) {
    console.error(err)
    process.exit(1)
  } else {
    console.log('Icons zijn gekopieerd')
  }
})
