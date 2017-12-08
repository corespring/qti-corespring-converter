var child_process = require('child_process');
var argv = require('minimist')(process.argv.slice(2));
var _ = require('lodash');
var fs = require('fs');
var path = require('path');
const { toArgArray, toArgString, converterArgs, run, escapeValues } = require('./utils');
var rimraf = require('rimraf');
const jsesc = require('jsesc');
const chalk = require('chalk');
const log = console.log;

var legacyLib = process.env.LEGACY_LIB || "/Users/edeustace/dev/executables/corespring/qti-corespring-converter/0.30";

log(chalk.yellow(`LEGACY_LIB: ${legacyLib}`));

const name = argv.compareName.toString();
const results = '.results';
const rootPath = path.resolve(path.join(results, name));
const resetLegacy = argv['reset-legacy'];


if (resetLegacy) {
  log(chalk.blue('removing legacy results...'));
  rimraf.sync(rootPath)
}

if (!fs.existsSync(results)) {
  fs.mkdirSync(results);
}

if (!fs.existsSync(rootPath)) {
  fs.mkdirSync(rootPath);
}

var legacyPathOut = path.join(rootPath, 'legacy.zip');
var legacyUnzippedDir = path.join(rootPath, 'legacy');

if (!fs.existsSync(legacyPathOut)) {
  log(chalk.green('building legacy zip to: ', legacyPathOut));
  const legacyArgsObject = converterArgs(argv, { output: legacyPathOut })
  run(
    'bin/qti-corespring-converter',
    toArgString(legacyArgsObject),
    legacyLib);
}

if (!fs.existsSync(legacyUnzippedDir)) {
  log(chalk.green('unzipping legacy zip to: ', legacyUnzippedDir));
  run('unzip', `${legacyPathOut} -d legacy`, rootPath);
}

/** now run the current projects code. */

log(chalk.red('removing sbt build assets'));
var latestPathOut = path.join(rootPath, 'latest.zip');
var latestDir = path.join(rootPath, 'latest');
rimraf.sync(latestPathOut);
rimraf.sync(latestDir);

const sbtArgs = converterArgs(argv, { output: path.resolve('.', latestPathOut) });

const escaped = escapeValues(sbtArgs);
const rawCmdString = `run ${toArgString(escaped)}`
const cmd = `"${jsesc(rawCmdString, { quotes: 'double' })}"`;

log(chalk.green('cmd: ', cmd));

const dir = path.resolve('..');

log(chalk.red('running sbt'));

rimraf.sync(path.join(rootPath, 'sbt.log'));

var sbtLogStream = fs.createWriteStream(path.join(rootPath, 'sbt.log'));

sbtLogStream.on('open', () => {

  run(`/usr/local/bin/sbt`, cmd, dir, sbtLogStream);

  if (!fs.existsSync(latestDir)) {
    run('unzip', 'latest.zip -d latest', rootPath);
  }

  try {
    log(chalk.yellow('running diff...'));
    run('/usr/bin/diff', '-r legacy/ latest/', rootPath);
  } catch (e) {
    log(e.stdout.toString())
  }
});