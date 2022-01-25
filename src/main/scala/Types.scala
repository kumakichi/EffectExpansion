package side.effect.free

import Wrappers.RichBody
import soot.{Local, SootMethod, Value, Unit => SootUnit}
import soot.jimple._

import scala.collection.mutable

case class Needle(procedure: SootMethod) {
  val instructions      = procedure.retrieveActiveBody().units
  var next: SootUnit    = instructions.getFirst
  def jmp(to: SootUnit) = next = to

  /** advance the needle
    * @return the needle should be interpreted
    */
  def advance = {
    val (afterward, current) = (instructions.getSuccOf(next), next)
    next = afterward
    current
  }
}

case class Scope(
    local: mutable.Map[String, Types],
    upper: Option[Scope],
    method: SootMethod,
    args: Array[Types],
    receiver: Option[Types]
) {
  val needle = Needle(method)
}

object Scope {
  implicit object StaticScope {
    val global: mutable.Map[String, Types] = mutable.Map()
  }
}

sealed trait Types {}
sealed class Primitive extends Types
sealed class Reference extends Types

// TODO: employ AnyVal
case class Ints(value: Int)         extends Primitive
case class Doubles(value: Double)   extends Primitive
case class Floats(value: Float)     extends Primitive
case class Longs(value: Long)       extends Primitive
case class Shorts(value: Short)     extends Primitive
case class Bytes(value: Byte)       extends Primitive
case class Booleans(value: Boolean) extends Primitive
case class Chars(value: Char)       extends Primitive

case class Arrays(value: Array[Any]) extends Reference
case class Strings(value: String)    extends Reference
case class Objects(value: AnyRef)    extends Reference

sealed class ValueSyntax

sealed class ScalarSyntaxes  extends ValueSyntax
sealed class MiscSyntax      extends ValueSyntax
sealed class ConstantsSyntax extends ValueSyntax
sealed class InvokeSyntax    extends ValueSyntax

object ScalarSyntaxes {
  object Bin {
    def unapply(arg: BinopExpr): Option[(Value, Value)] = Some(arg.getOp1, arg.getOp2)
  }

  implicit class ArrayReference(expr: ArrayRef)                     extends MiscSyntax
  implicit class NewArrayExpression(expr: NewArrayExpr)             extends MiscSyntax
  implicit class NewMultiArrayExpression(expr: NewMultiArrayExpr)   extends MiscSyntax
  implicit class NewExpression(expr: NewExpr)                       extends MiscSyntax
  implicit class ArrayLengthExpression(expr: LengthExpr)            extends MiscSyntax
  implicit class InstanceFieldReference(expr: InstanceFieldRef)     extends MiscSyntax
  implicit class LocalReference(expr: Local)                        extends MiscSyntax
  implicit class ParameterReference(expr: ParameterRef)             extends MiscSyntax
  implicit class CaughtExceptionReference(expr: CaughtExceptionRef) extends MiscSyntax
  implicit class ThisReference(expr: ThisRef)                       extends MiscSyntax
  implicit class StaticFieldReference(expr: StaticFieldRef)         extends MiscSyntax
  implicit class InstanceOfExpression(expr: InstanceOfExpr)         extends MiscSyntax

  implicit class ApplyDoubles(expr: DoubleConstant)     extends ConstantsSyntax
  implicit class ApplyLongs(expr: LongConstant)         extends ConstantsSyntax
  implicit class ApplyInts(expr: IntConstant)           extends ConstantsSyntax
  implicit class ApplyFloats(expr: FloatConstant)       extends ConstantsSyntax
  implicit class ApplyNulls(expr: NullConstant)         extends ConstantsSyntax
  implicit class ApplyStrings(expr: StringConstant)     extends ConstantsSyntax
  implicit class ApplyClasses(expr: ClassConstant)      extends ConstantsSyntax
  implicit class ApplyMethodHandles(expr: MethodHandle) extends ConstantsSyntax

  implicit class InterfaceInvoke(expr: InterfaceInvokeExpr) extends InvokeSyntax
  implicit class StaticInvoke(expr: StaticInvokeExpr)       extends InvokeSyntax
  implicit class SpecialInvoke(expr: SpecialInvokeExpr)     extends InvokeSyntax
  implicit class VirtualInvoke(expr: VirtualInvokeExpr)     extends InvokeSyntax
  implicit class InstanceInvoke(expr: InstanceInvokeExpr)   extends InvokeSyntax
  implicit class DynamicInvoke(expr: DynamicInvokeExpr)     extends InvokeSyntax

  implicit class Add(add: AddExpr)    extends ScalarSyntaxes
  implicit class Sub(sub: SubExpr)    extends ScalarSyntaxes
  implicit class Mul(mul: MulExpr)    extends ScalarSyntaxes
  implicit class Div(div: DivExpr)    extends ScalarSyntaxes
  implicit class And(and: AndExpr)    extends ScalarSyntaxes
  implicit class Cmp(cmp: CmpExpr)    extends ScalarSyntaxes
  implicit class Cmpg(cmpg: CmpgExpr) extends ScalarSyntaxes
  implicit class Eq(eq: EqExpr)       extends ScalarSyntaxes
  implicit class Ge(ge: GeExpr)       extends ScalarSyntaxes
  implicit class Gt(gt: GtExpr)       extends ScalarSyntaxes
  implicit class Lt(lt: LtExpr)       extends ScalarSyntaxes
  implicit class Le(le: LeExpr)       extends ScalarSyntaxes
  implicit class Ne(ne: NeExpr)       extends ScalarSyntaxes
  implicit class Rem(rem: RemExpr)    extends ScalarSyntaxes
  implicit class Shl(shl: ShlExpr)    extends ScalarSyntaxes
  implicit class Shr(shr: ShrExpr)    extends ScalarSyntaxes
  implicit class Ushr(ushr: UshrExpr) extends ScalarSyntaxes
  implicit class Xor(xor: XorExpr)    extends ScalarSyntaxes
  implicit class Neg(neg: NegExpr)    extends ScalarSyntaxes
}

object StatementSyntax {
  class BreakPoint(expr: BreakpointStmt)
  class Invoke(expr: InvokeExpr)
  class Assign(expr: AssignStmt)
  class Identity(expr: IdentityStmt)
  class EnterMonitor(expr: EnterMonitorStmt)
  class ExitMonitor(expr: ExitMonitorStmt)
  class Goto(expr: GotoStmt)
  class If(expr: IfStmt)
  class LookUpSwitch(expr: LookupSwitchStmt)
  class TableSwitch(expr: TableSwitchStmt)
  class Nop(expr: NopStmt)
  class Return(expr: ReturnStmt)
  class ReturnVoid(expr: ReturnVoidStmt)
  class Throw(expr: ThrowStmt)
}
