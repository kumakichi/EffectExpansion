package side.effect.free

import org.slf4j.LoggerFactory

object Predef {
  val logger                                                    = LoggerFactory.getLogger("EffectExpansion")
  def debug(tag: String, message: String)                       = LoggerFactory.getLogger(tag).debug(message)
  def info(tag: String, message: String)                        = LoggerFactory.getLogger(tag).info(message)
  def error(tag: String, message: String, throwable: Throwable) = LoggerFactory.getLogger(tag).error(message, throwable)
  def raise(tag: String, message: String) = {
    LoggerFactory.getLogger(tag).error(message)
    throw new IllegalStateException(message)
  }
}
