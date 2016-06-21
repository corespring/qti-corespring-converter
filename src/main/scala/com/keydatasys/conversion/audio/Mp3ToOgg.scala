package com.keydatasys.conversion.audio

object Mp3ToOgg {

  private val PathToFfmpeg = "/usr/local/bin/ffmpeg"  // this is the default with OS X + homebrew

  def convert(input: String, output: String) = {
    val conversionProcess = new ProcessBuilder(PathToFfmpeg, "-i", input, "-b:a", "192k", output)
    val process = conversionProcess.start()
    assert(0 == process.waitFor(), "the process should have completed with exit code '0'")
  }

}
