package com.keydatasys.conversion.audio

import java.io._

object Mp3ToOgg {

  private val PathToFfmpeg = "/usr/local/bin/ffmpeg"  // this is the default with OS X + homebrew

  def convert(source: File, target: String) = {
    val conversionProcess = new ProcessBuilder(PathToFfmpeg, "-y", "-i", source.getAbsolutePath, "-b:a", "192k", target)
    conversionProcess.redirectErrorStream()
    val process = conversionProcess.start()
    assert(0 == process.waitFor(), "the process should have completed with exit code '0'")
  }

}
