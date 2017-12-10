var argv = require('minimist')(process.argv.slice(2));
var fs = require('fs');
var path = require('path');
const { run } = require('./utils');
const log = console.log;
const name = argv.compareName;
const glob = require('glob');

const dumpXhtml = (dir) => {
  const root = path.join('.results', name, dir, '**');

  const [legacyPath] = glob.sync(path.join(root, 'player-definition.json'));
  const legacy = require(`./${legacyPath}`);
  const [dataDir] = glob.sync(path.join(root, 'data'));
  const htmlPath = path.join(`./${dataDir}/index.html`);
  fs.writeFileSync(htmlPath, legacy.xhtml, 'UTF-8');
  return htmlPath;
}

const legacyPath = dumpXhtml('legacy');
const latestPath = dumpXhtml('latest');

run('/usr/bin/diff', `${legacyPath} ${latestPath}`);