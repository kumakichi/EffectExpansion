package side.effect.free

import Predef.{debug, error, raise}
import Wrappers.{RichBody, SAssignStmt, SLocal}

import soot.jimple._
import soot.{Local, SootMethod, Value, Unit => SootUnit}

import scala.collection.mutable
import scala.util.control.Exception.catching

case class Needle(procedure: SootMethod) {
  val instructions           = procedure.retrieveActiveBody().units
  var next: Option[SootUnit] = Option(instructions.getFirst)
  def jmp(to: SootUnit)      = next = Some(to)

  /** advance the needle,
    * [[next]] was always non-empty, cause each call to advance will check its presents
    *  [[next]] may be None after [[advance]] was executed
    *
    * @return the needle should be interpreted, never None
    */
  def advance: SootUnit = {
    val following = for {
      next <- next // always success
      to   <- Some(instructions.getSuccOf(next)) // maybe fail
    } yield to

    val instr = next.get
    next = following
    instr
  }
}

case class Scope(
    local: mutable.Map[Local, Types],
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

case class Arrays(value: mutable.Seq[Types]) extends Reference
case class Strings(value: String)            extends Reference
case class Objects(value: AnyRef)            extends Reference
case object Undefined                        extends Reference

trait Syntax {}

sealed trait StatementSyntax extends Syntax {
  def eval(scope: Scope): Scope = ???
}
sealed trait ValueSyntax extends Syntax {
  def eval(scope: Scope): (Option[Types], Scope) = ???
}

sealed trait ScalarSyntaxes  extends ValueSyntax
sealed trait MiscSyntax      extends ValueSyntax
sealed trait ConstantsSyntax extends ValueSyntax
sealed trait InvokeSyntax    extends ValueSyntax

object StatementSyntax {
  class BreakPoint(expr: BreakpointStmt) extends StatementSyntax
  class Invoke(expr: InvokeExpr)         extends StatementSyntax
  class Assign(expr: AssignStmt) extends StatementSyntax {
    override def eval(scope: Scope) = {
      val SAssignStmt(left @ SLocal(name, _), right) = expr
      val (resolved, _) = right match {
        case value: ValueSyntax => value.eval(scope)
        case _                  => raise("Assign.eval", s"invalid value type:$right")
      }
      resolved.foreach(scope.local(left) = _)
      scope
    }
  }
  class Identity(expr: IdentityStmt)         extends StatementSyntax
  class EnterMonitor(expr: EnterMonitorStmt) extends StatementSyntax
  class ExitMonitor(expr: ExitMonitorStmt)   extends StatementSyntax
  class Goto(expr: GotoStmt)                 extends StatementSyntax
  class If(expr: IfStmt)                     extends StatementSyntax
  class LookUpSwitch(expr: LookupSwitchStmt) extends StatementSyntax
  class TableSwitch(expr: TableSwitchStmt)   extends StatementSyntax
  class Nop(expr: NopStmt)                   extends StatementSyntax
  class Return(expr: ReturnStmt)             extends StatementSyntax
  class ReturnVoid(expr: ReturnVoidStmt)     extends StatementSyntax
  class Throw(expr: ThrowStmt)               extends StatementSyntax
}

object MiscSyntax {
  implicit class ArrayReference(expr: ArrayRef) extends MiscSyntax {
    override def eval(scope: Scope) = {
      val ArrayReference(base @ SLocal(_, _), index, _) = expr
      val immediate = for {
        index <- index match {
          case value @ SLocal(_, _) =>
            catching(classOf[Throwable]) opt {
              scope.local(value).asInstanceOf[Ints].value
            }
          case value: IntConstant => Some(value.value)
        }
        value <- catching(classOf[Throwable]) opt scope.local(base).asInstanceOf[Arrays].value(index)
      } yield value
      debug("ArrayReference", s"$base($index) == $immediate")
      immediate -> scope
    }
  }

  object ArrayReference {
    def unapply(arg: ArrayRef) = Some(arg.getBase, arg.getIndex, arg.getType)
  }

  implicit class NewArrayExpression(expr: NewArrayExpr) extends MiscSyntax {
    override def eval(scope: Scope) = {
      val NewArrayExpression(typ, size) = expr
      debug("NewArrayExpression", s"type:$typ size:$size")
      Some(Arrays(mutable.Seq[Types]())) -> scope
    }
  }

  object NewArrayExpression {
    def unapply(arg: NewArrayExpr) = Some(arg.getType, arg.getSize)
  }

  implicit class NewMultiArrayExpression(expr: NewMultiArrayExpr) extends MiscSyntax {
    override def eval(scope: Scope) = {
      val NewMultiArrayExpression(typ, sizes) = expr
      debug("NewMultiArrayExpression", s"type:$typ sizes:$sizes")
      Some(Arrays(mutable.Seq[Types]())) -> scope
    }
  }

  object NewMultiArrayExpression {
    def unapply(arg: NewMultiArrayExpr) = Some(arg.getType, arg.getSizes)
  }

  implicit class NewExpression(expr: NewExpr)                       extends MiscSyntax
  implicit class ArrayLengthExpression(expr: LengthExpr)            extends MiscSyntax
  implicit class InstanceFieldReference(expr: InstanceFieldRef)     extends MiscSyntax
  implicit class LocalReference(expr: Local)                        extends MiscSyntax
  implicit class ParameterReference(expr: ParameterRef)             extends MiscSyntax
  implicit class CaughtExceptionReference(expr: CaughtExceptionRef) extends MiscSyntax
  implicit class ThisReference(expr: ThisRef)                       extends MiscSyntax
  implicit class StaticFieldReference(expr: StaticFieldRef)         extends MiscSyntax
  implicit class InstanceOfExpression(expr: InstanceOfExpr)         extends MiscSyntax
}

object ConstantsSyntax {
  implicit class ApplyDoubles(expr: DoubleConstant)     extends ConstantsSyntax
  implicit class ApplyLongs(expr: LongConstant)         extends ConstantsSyntax
  implicit class ApplyInts(expr: IntConstant)           extends ConstantsSyntax
  implicit class ApplyFloats(expr: FloatConstant)       extends ConstantsSyntax
  implicit class ApplyNulls(expr: NullConstant)         extends ConstantsSyntax
  implicit class ApplyStrings(expr: StringConstant)     extends ConstantsSyntax
  implicit class ApplyClasses(expr: ClassConstant)      extends ConstantsSyntax
  implicit class ApplyMethodHandles(expr: MethodHandle) extends ConstantsSyntax
}

object InvokeSyntax {
  implicit class InterfaceInvoke(expr: InterfaceInvokeExpr) extends InvokeSyntax
  implicit class StaticInvoke(expr: StaticInvokeExpr)       extends InvokeSyntax
  implicit class SpecialInvoke(expr: SpecialInvokeExpr)     extends InvokeSyntax
  implicit class VirtualInvoke(expr: VirtualInvokeExpr)     extends InvokeSyntax
  implicit class InstanceInvoke(expr: InstanceInvokeExpr)   extends InvokeSyntax
  implicit class DynamicInvoke(expr: DynamicInvokeExpr)     extends InvokeSyntax
}

object ScalarSyntaxes {
  object Bin {
    def unapply(arg: BinopExpr): Option[(Value, Value)] = Some(arg.getOp1, arg.getOp2)
  }

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
