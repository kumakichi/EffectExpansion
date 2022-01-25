package side.effect.free
import Predef._
import Wrappers.RichBody

import soot.options.Options
import soot.{SootMethod, Unit => SootUnit}

case class Interpreter(options: Options, entry: SootMethod) {
  def interpret(): Unit = {
    var prologue: Option[Needle] = Some(Needle(entry.retrieveActiveBody().units.getFirst, entry))
    debug("interpret", s"starting vm: $prologue")
    while (prologue.isDefined) {
      debug("interpret", s"starting vm: $prologue")
      prologue = for {
        prologue <- prologue
        prologue <- interpret(prologue)
      } yield prologue
    }
  }
  def interpret(needle: Needle): Option[Needle] = ???
}
