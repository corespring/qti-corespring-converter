const child_process = require('child_process');
const _ = require('lodash');
const path = require('path');
const jsesc = require('jsesc');
const log = console.log;
const chalk = require('chalk');

exports.run = (cmd, args, cwd, stream) => {
  log(chalk.yellow('cmd', cmd, '\nargs: ', args, '\ncwd: ', cwd));

  const opts = { cwd, stdio: [0, stream || 1, 2] }
  return child_process.execSync(`${cmd} ${args}`, opts);
}

exports.escapeValues = (o) => {
  return _.mapValues(o, v => {
    if (_.isObject(v)) {
      return `"${jsesc(JSON.stringify(v), { quotes: 'double' })}"`;
    } else {
      return v;
    }
  });
}

// exports.toOpts = (arr) => _.flatten(arr.map(([k, v]) => [`--${k}`, v]));
exports.toArgString = (o) => {
  log(chalk.blue(JSON.stringify(o)))
  return _.reduce(o, (acc, v, k) => {
    return acc += `--${k} ${_.isObject(v) ? JSON.stringify(v) : v} `;
  }, '');
}

exports.toArgArray = (o) => _.toPairs(o).map(([k, v]) => [`--${k}`, _.isObject(v) ? JSON.stringify(v) : v]);

exports.converterArgs = (argv, extra) => {

  const common = {
    input: path.resolve(argv.input),
    vendor: argv.vendor
  }

  if (argv.sourceIdList) {
    common.sourceIdList = path.resolve(argv.sourceIdList);
  }

  if (argv.sourceId) {
    common.sourceId = argv.sourceId;
  }

  if (common.vendor === 'kds') {
    const o = { scoringType: argv['kds-type'] || 'SBAC' };
    const escaped = jsesc(JSON.stringify(o), { quotes: 'double' });
    common.metadata = `"${escaped}"`;
  }

  return Object.assign(common, extra);
}