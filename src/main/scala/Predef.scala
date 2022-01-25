package side.effect.free

import org.slf4j.LoggerFactory

object Predef {
  val logger                                                   = LoggerFactory.getLogger("EffectExpansion")
  def debug(tag: String, message: String)                      = LoggerFactory.getLogger(tag).debug(message)
  def info(tag: String, message: String)                       = LoggerFactory.getLogger(tag).info(message)
  def warn(tag: String, message: String, throwable: Throwable) = LoggerFactory.getLogger(tag).error(message, throwable)
}
