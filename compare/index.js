var child_process = require('child_process');
var argv = require('minimist')(process.argv.slice(2));
var _ = require('lodash');
var fs = require('fs');
var path = require('path');
const utils = require('./utils');
var rimraf = require('rimraf');


var legacyLib = process.env.LEGACY_LIB ||  "/Users/edeustace/dev/executables/corespring/qti-corespring-converter/0.30";

delete argv._
var name = argv.compareName.toString();
delete argv.compareName;
console.log(argv);
console.log('name', name);

var kdsType = argv['kds-type'];

argv.metadata = JSON.stringify({ scoringType: kdsType });

delete argv['kds-type'];

var results = '.results';


var resetLegacy = argv['reset-legacy'];

delete argv['reset-legacy'];

if(resetLegacy){
  rimraf.sync(path.join(results, name))
}

if(!fs.existsSync(results)){
  fs.mkdirSync(results);
}

if(!fs.existsSync(path.join(results, name))){
  fs.mkdirSync(path.join(results, name));
}

var legacyPathOut = path.resolve(path.join(results, name, 'legacy.zip'));
var legacyUnzippedDir = path.resolve(path.join(results, name, 'legacy'));

if(!fs.existsSync(legacyPathOut)){
  legacyArgs = _.toPairs(argv).concat([['output',legacyPathOut]]);
  console.log('legacyArgs', legacyArgs);
  var finalArgs = _.flatten(legacyArgs.map( ([k,v]) => [`--${k}`, v]));
  console.log(finalArgs);
  var {status} = utils.run('bin/qti-corespring-converter', finalArgs, legacyLib);
  if(status !== 0){
    console.log('err!!');
  }
}

if(!fs.existsSync(legacyUnzippedDir)){
  var {status} = utils.run('unzip', [legacyPathOut, '-d', 'legacy'], path.join(results, name));
  console.log('unzip status: ', status)
}



//child_process.spawnSync('bin/qti-corespring-converter', args, {cwd: legacyLib});
//
//console.dir(argv);


