package side.effect.free
import Wrappers.RichBody

import soot.options.Options
import soot.{SootMethod, Unit => SootUnit}

class Stacks()
class Locals()
class Heaps()

case class Needle(needle: SootUnit, withIn: SootMethod) {
  val instructions      = withIn.retrieveActiveBody().units
  def jmp(to: SootUnit) = Needle(to, withIn)
  def next              = Needle(instructions.getSuccOf(needle), withIn)
}

case class Interpreter(options: Options, entry: SootMethod) {
  def interpret(): Unit = {
    var prologue: Option[Needle] = Some(Needle(entry.retrieveActiveBody().units.getFirst, entry))
    println(s"Starting Vm: $prologue")
    while (prologue.isDefined) {
      prologue = for {
        prologue <- prologue
        prologue <- interpret(prologue)
      } yield prologue
    }
  }

  def interpret(needle: Needle): Option[Needle] = ???

}

sealed trait Types
sealed class Primitive extends Types

// primitive types representation
// TODO: employ AnyVal
case class Ints(value: Int)         extends Primitive
case class Doubles(value: Double)   extends Primitive
case class Floats(value: Float)     extends Primitive
case class Longs(value: Long)       extends Primitive
case class Shorts(value: Short)     extends Primitive
case class Bytes(value: Byte)       extends Primitive
case class Booleans(value: Boolean) extends Primitive
case class Chars(value: Char)       extends Primitive

// reference type representation
case class Arrays(value: Array[Any])
case class Reference(value: AnyRef)
