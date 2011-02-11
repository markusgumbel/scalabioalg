package net.gumbix.util

/**
 * @author Markus Gumbel (m.gumbel@hs-mannheim.de)
 */

trait Logger {
  var logLevel = true // TODO use enum.
  def log(s: String) {
    if (logLevel) print(s)
  }

  def logln(s: String) {
    log(s + "\n")
  }

  def logln() {
    log("\n")
  }
}