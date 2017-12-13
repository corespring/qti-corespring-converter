# 1.0.0

This branch has an number of changes over 0.30.x

* no entity escaping
* no CDATA escaping
* better markup preparation
* read resources only once
* parse only once

0.30 has issues because it escapes the contents of CDATA nodes and puts them into the xml tree. This can break parsing.

1.0.0 fixes this by not touching the CDATA until the very end when we are preparing the xhtml string.This allows the conversion to complete and only escapes cdata once that is done.

This branch is nearly ready to be merged the main outstanding tasks are: 

* check all the transformers are not using `node.toString` (should be `node.text` always).
* check imports from the other vendors (measuredprogress, progresstesting)

> There is a compare tool that will do 2 conversions of an input zip and then diff them.. one version is from 0.30.3 and the other is from this branch. This allows you to quickly see what differences there are.

* after an import to staging - a pretty thorough check of the imported items should be done.

I'm parking this for now because I don't have the free time to do the above and really the change should be managed along with the given vendor so they are happy.

