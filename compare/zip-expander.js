const { run } = require('./utils');
const { readJsonSync, writeJsonSync, writeFileSync } = require('fs-extra');
const log = console.log;
const chalk = require('chalk');
const path = require('path');
const glob = require('glob');

module.exports = function (zip, dirname, cwd) {
  log(chalk.green(`[zip-expander] unzipping ${zip} -> ${dirname} in ${cwd}`));
  run('unzip', `${zip} -d ${dirname}`, cwd);
  const expandedPath = path.join(cwd, dirname);
  const defs = glob.sync(`${expandedPath}/**/player-definition.json`);

  defs.forEach((pd) => {
    const info = readJsonSync(pd);
    const cs = info.customScoring
    if (cs) {
      delete info.customScoring;
      writeFileSync(path.resolve(pd, '..', 'custom-scoring.js'), cs);
    }
    const html = info.xhtml;
    delete info.xhtml;
    writeJsonSync(pd, info, { spaces: 2 });
    writeFileSync(path.resolve(pd, '..', 'index.html'), html, 'utf8');
  });

  return expandedPath;
}