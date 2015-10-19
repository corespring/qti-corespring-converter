### qti-corespring-converter

`qti-corespring-converter` is a Scala library that translates QTI into CoreSpring's item format. This project is 
currently in a WIP state.

#### Usage

You can use `qti-corespring-converter` from the command line:

    sbt "run --input qti.zip --output json.zip --vendor kds --metadata \"{\\\"scoringType\\\": \\\"PARCC\\\"}\""
    

Or utilize it directly from Scala code:

    import org.corespring.conversion.qti.{ QtiTransformer => CoreSpringQtiTransformer }
    import com.keydatasys.conversion.qti.{ QtiTransformer => KDSQtiTransformer }

    
#### Testing

    sbt test