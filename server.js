const fs = require('fs')
const express = require('express')
const bodyParser = require('body-parser')

const PORT = 8080
const RESULTS_FILE = 'results.json'

const app = express()

let state = []

app.use(bodyParser.urlencoded({ extended: true }))

app.post('/', (req, res) => {
  handle(req.body)
  res.send('ok\n')
})

app.listen(PORT, () =>
  console.log('Listening on', PORT)
)

const handle = payload => {
  const time = parseFloat(payload.time)
  const result = Object.assign(payload, { time })
  state = [result, ...state]

  console.log(result)

  fs.writeFile(RESULTS_FILE, JSON.stringify(state, null, 2), err => err && console.error(err))
}
