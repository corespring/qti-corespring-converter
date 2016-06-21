### qti-corespring-converter

`qti-corespring-converter` is a Scala library that translates QTI into CoreSpring's item format. This project is 
currently in a WIP state.

#### Installation

You will need `ffmpeg` to perform Ogg Vorbis conversions from mp3s. To install this on Mac OS:

    brew install ffmpeg --with-libvpx --with-theora --with-libogg --with-libvorbis

Make sure you have the requisite audio codecs (`libvorbis`, specifically), otherwise the files cannot be converted.

#### Usage

You can use `qti-corespring-converter` from the command line:

    sbt "run --input qti.zip --output json.zip --vendor kds --metadata \"{\\\"scoringType\\\": \\\"PARCC\\\"}\""
    

Or utilize it directly from Scala code:

    import org.corespring.conversion.qti.{ QtiTransformer => CoreSpringQtiTransformer }
    import com.keydatasys.conversion.qti.{ QtiTransformer => KDSQtiTransformer }

    
#### Testing

    sbt test