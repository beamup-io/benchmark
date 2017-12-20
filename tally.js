const fs = require('fs')
const groupBy = require('lodash/fp/groupBy')
const mapValues = require('lodash/fp/mapValues')
const min = require('lodash/fp/min')
const max = require('lodash/fp/max')
const mean = require('lodash/fp/mean')
const stdev = require('compute-stdev')

const results = JSON.parse(fs.readFileSync('results.json'))

const seconds = t => Math.round(t / 1000.0)

const aggregate = values => {
  return {
    min: seconds(min(values)),
    max: seconds(max(values)),
    mean: seconds(mean(values)),
    stdev: (stdev(values) / 1000.0).toFixed(1),
    n: values.length
  }
}

const bySubject = groupBy('subject')(results)
const byStep = mapValues(groupBy('step'))(bySubject)
const plain = mapValues(mapValues(x => x.map(y => y.time)))(byStep)
const aggregates = mapValues(mapValues(aggregate))(plain)

const providers = {
  'CircleCI': { name: 'CircleCI', host: 'Con.' },
  'CodeshipPro': { name: 'Codeship', host: 'Con.' },
  'Semaphore': { name: 'Semaphore', host: 'Con.' },
  'Shippable': { name: 'Shippable', host: 'Con.' },
  'TravisContainer': { name: 'Travis CI', host: 'Con.' },
  'TravisVM': { name: 'Travis CI', host: '\\acrshort{vm}' },
  'Wercker': { name: 'Wercker', host: 'Con.' }
}

const toRowData = subject => Object.assign(providers[subject], {
  steps: aggregates[subject]
})

const toLatexCell = c => {
  return `${c.mean} & ${c.stdev}`
}

const toLatexRow = r => {
  const cols = [
    r.name,
    r.host,
    // Column order: Install | empty cache | primed cache
    toLatexCell(r.steps['1_install']),
    toLatexCell(r.steps['2_full_build_without_cache']),
    toLatexCell(r.steps['5_upgrade_build_without_cache']),
    toLatexCell(r.steps['3_full_build_with_cache']),
    toLatexCell(r.steps['4_upgrade_build_with_cache'])
  ]
  return cols.join(' & ') + ' \\\\'
}

const latexRows = Object.keys(aggregates)
  .sort()
  .map(toRowData)
  .map(toLatexRow)
  .join('\n  \\hline\n')

const latexSummaryRow = ``

const latexTable = `
% auto generated file
\\newcommand*\\mean{$\\bar{s}$}
\\begin{tabular}{ l l r r|r r|r r|r r|r r|}
  & & & & \\multicolumn{4}{c|}{Empty cache} & \\multicolumn{4}{c|}{Primed cache} \\\\
  & & \\multicolumn{2}{c|}{Install} & \\multicolumn{2}{c|}{Initial} & \\multicolumn{2}{c|}{Upgrade} & \\multicolumn{2}{c|}{Initial} & \\multicolumn{2}{c|}{Upgrade} \\\\
  \\acrshort{ci} Provider & Host & \\mean & $\\sigma$ & \\mean & $\\sigma$ & \\mean & $\\sigma$ & \\mean & $\\sigma$ & \\mean & $\\sigma$ \\\\
  \\hhline{====|==|==|==|==|}
  ${latexRows}
  \\hhline{====|==|==|==|==|}
  ${latexSummaryRow}
\\end{tabular}
`

console.log(latexTable)
