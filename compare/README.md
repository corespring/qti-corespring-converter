# Compare

A script that runs 2 different versions of the qti-corespring-converter,
unzips the output and diffs the files. This is so you can quickly get an idea of what
changes are in the emitted json of the current project.

The benchmark lib is 0.30 by default.

## Running

```bash
cd compare
yarn install
node index.js --compareName set-one --input ~/dev/github/corespring/kds-processor/target/KDS-SBAC.zip --sourceIdList ./sourceIdList.txt --vendor kds --kds-type SBAC
# --reset-legacy - add this to remove the legacy build results
```

The changes come back as (legacy is before, latest is latest):

```bash
diff -r legacy/kds_5453b4e4e4b05f38dd6440a8/663934/player-definition.json latest/kds_5453b4e4e4b05f38dd6440a8/663934/player-definition.json
14c14
<           "graphPadding" : 50,
---
>           "graphPadding" : 500,
```