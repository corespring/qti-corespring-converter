var child_process = require('child_process');
var argv = require('minimist')(process.argv.slice(2));
var _ = require('lodash');
var fs = require('fs');
var path = require('path');
const utils = require('./utils');
var rimraf = require('rimraf');
const jsesc = require('jsesc');
const chalk = require('chalk');
const log = console.log;

var legacyLib = process.env.LEGACY_LIB ||  "/Users/edeustace/dev/executables/corespring/qti-corespring-converter/0.30";


log(chalk.yellow(`LEGACY_LIB: ${legacyLib}`));

delete argv._
var name = argv.compareName.toString();
delete argv.compareName;

var kdsType = argv['kds-type'];

argv.metadata = JSON.stringify({ scoringType: kdsType });

delete argv['kds-type'];

var results = '.results';

const rootPath = path.resolve(path.join(results, name));

var resetLegacy = argv['reset-legacy'];

delete argv['reset-legacy'];

argv.sourceIdList = path.resolve(argv.sourceIdList);

if(resetLegacy){
  log(chalk.blue('removing legacy results...'));
  rimraf.sync(rootPath)
}

if(!fs.existsSync(results)){
  fs.mkdirSync(results);
}


if(!fs.existsSync(rootPath)){
  fs.mkdirSync(rootPath);
}

var legacyPathOut = path.join(rootPath, 'legacy.zip');
var legacyUnzippedDir = path.join(rootPath, 'legacy');

if(!fs.existsSync(legacyPathOut)){
  log(chalk.green('building legacy zip to: ', legacyPathOut));
  legacyArgs = _.toPairs(argv).concat([['output',legacyPathOut]]);
  var finalArgs = utils.toOpts(legacyArgs);
  var {status} = utils.run('bin/qti-corespring-converter', finalArgs, legacyLib);
  if(status !== 0){
    log(chalk.red('err!!'));
  }
}

if(!fs.existsSync(legacyUnzippedDir)){
  log(chalk.green('unzipping legacy zip to: ', legacyUnzippedDir));
  var {status} = utils.run('unzip', [legacyPathOut, '-d', 'legacy'], path.join(results, name));
  log('unzip status: ', status)
}


/** now run the current projects code. */

log(chalk.red('removing sbt build assets'));
rimraf.sync(path.join(rootPath, 'latest.zip'));
rimraf.sync(path.join(rootPath, 'latest'));

var preprocess = _.cloneDeep(argv);
preprocess.metadata = `"${jsesc(preprocess.metadata, {quotes: 'double'})}"`

var latestPathOut = path.join(rootPath, 'latest.zip');
var latestDir = path.join(rootPath, 'latest');

var latestArgs = _.toPairs(preprocess).concat([['output', path.resolve('.', latestPathOut)]]);
var argString = `run ${utils.toOpts(latestArgs).join(' ')}`;
var escaped = jsesc(argString, {quotes: 'double'});
log(chalk.magenta('cmd: ', escaped));

var dir = path.resolve('..');

log(chalk.red('running sbt'));
child_process.execSync(`/usr/local/bin/sbt "${escaped}"`, {cwd:dir});


if(!fs.existsSync(latestDir)){
  var {status} = utils.run('unzip', ['latest.zip', '-d', 'latest'], path.join(results, name));
  log('unzip status: ', status)
}

try {

  log(chalk.yellow('running diff...'));
  var result = child_process.execSync(
  '/usr/bin/diff -r legacy/ latest/',
  {
    cwd: path.resolve(path.join(results, name))
  });

  log(result.toString());
} catch (e){
  log(e.stdout.toString())
}
