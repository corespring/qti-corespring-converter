var child_process = require('child_process');


exports.run = (cmd, args, dir) => {
  return child_process.spawnSync(
  cmd,
  args,
  {
    cwd: dir,
    stdio: [
      0, // Use parent's stdin for child
      'pipe', // Pipe child's stdout to parent
      'pipe'
    ]});
}