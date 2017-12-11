# 1. CDATA is being escaped and parsed as XML - but this breaks parsing

* Some content has <x><![CDATA[a < b]]></x>, this is escaped and it breaks the xml parsing.
* In the xhtml section (<itemBody>) we have invalid xhtml markup like `<audio controls></audio>`

The problems with the converter right now are:
* It treats all nodes equally,  removing CDATA and escaping
* It parses string to xml multiple times in different places and does the same work over and over again making it hard to control the processing.

## Full Fix

1. Don't escape CDATA - honor it (except when looking for resources in the markup).
1. We need to be able to parse the contents of <itemBody> as xml so we can convert certain tags within it. so we need to:
  1. remove all CDATA in <itemBody>
  2. fix the markup using jsoup
  3. parse the fixed markup as part of the overall qti - Use `scala-xml` `XHTMLParser` so we don't have to escape entities.
2. Don't escape CDATA anywhere else
3. make sure the transformers use `text` instead of `toString`


## (Slightly) Quicker Fix

1. We need to be able to parse the contents of <itemBody> as xml so we can convert certain tags within it. so we need to:
  1. remove all CDATA in <itemBody>
  2. fix the markup using jsoup
  3. parse the fixed markup as part of the overall qti (do the stupid escape entities thing for now)
2. Don't escape CDATA anywhere else
3. make sure the transformers use `text` instead of `toString`


## Test tool

We need a tool that we can use to test the output of the refactored library compared to previous versions.

```
# create content from 0.30
./bin/qti-corespring-converter --input X .... --output 0.30.zip
# create content from updated lib
sbt "run --input X ... --output latest.zip"
# compare the output of each
./bin/compare-zip-contents -a 0.30.zip -b latest.zip --out 0.30-latest.diff
> summary - everything is the same (or X files are different)...
```

Check:
* audio transformer picks up comp and markup is correctly emitted
* video and audio tags are corrected by jsoup
* progress testing escaped entities
* <br/> to <br></br>?
* -> 677198.xml - has graphicGapMatchInteraction - check images are ok.
* ->