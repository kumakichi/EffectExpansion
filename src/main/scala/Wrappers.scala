package side.effect.free

import soot.jimple._
import soot.jimple.spark.pag.PAG
import soot.jimple.toolkits.callgraph.{CallGraph, ContextSensitiveCallGraph, ReachableMethods}
import soot.jimple.toolkits.pointer.SideEffectAnalysis
import soot.options.Options
import soot.tagkit._
import soot.toolkits.exceptions.ThrowAnalysis
import soot.util._
import soot.{Unit => SootUnit, _}

import java.io.File
import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.util.Try

object Wrappers {

  private def ifToOption[T](condition: => Boolean, positiveResult: => T): Option[T] = if (condition) Some(positiveResult) else None

  implicit class RichNumerable(val n: Numberable) extends AnyVal {
    @inline def number: Int = n.getNumber

    @inline def number_=(newNumber: Int): Unit = n.setNumber(newNumber)
  }

  object SSootClass {
    def apply(
        name: String,
        modifiers: Int = Modifier.PUBLIC,
        fields: Iterable[SootField] = Iterable(),
        interfaces: Iterable[SootClass] = Iterable(),
        superClass: Option[SootClass] = None,
        methods: Iterable[SootMethod] = Iterable(),
        outerClass: Option[SootClass] = None,
        annotations: Iterable[AnnotationTag] = Iterable()
    ): SootClass = {
      val sc = new SootClass(name, modifiers)
      superClass.foreach(sc.superclass = _)
      outerClass.foreach(sc.outerClass = _)
      fields.foreach(sc.addField)
      interfaces.foreach(sc.addInterface)
      methods.foreach(sc.addMethod)
      if (annotations.nonEmpty) {
        sc.addTag(SVisibilityAnnotationTag(annotations))
      }
      sc
    }
  }

  implicit class RichSootClass(val v: SootClass) extends AnyVal {
    @inline def name = v.getName

    @inline def name_=(newName: String) = v.setName(newName)

    @inline def packageName: String = v.getPackageName

    @inline def shortName: String = v.getShortName

    @inline def modifiers: Int = v.getModifiers

    @inline def modifiers_=(mods: Int) = v.setModifiers(mods)

    @inline def fields: Chain[SootField] = v.getFields

    @inline def fields_+=(newField: SootField) = v.addField(newField)

    @inline def interfaces: Chain[SootClass] = v.getInterfaces

    @inline def interfaces_=(interfaces: Iterable[SootClass]) = {
      v.getInterfaces.foreach(v.removeInterface)
      interfaces.foreach(v.addInterface)
    }

    @inline def interfaces_+=(newInterface: SootClass) = v.addInterface(newInterface)

    @inline def superclass: SootClass = v.getSuperclass

    @inline def superclass_=(sc: SootClass) = v.setSuperclass(sc)

    @inline def superClassOpt = ifToOption(v.hasSuperclass, v.getSuperclass)

    @inline def methods = v.getMethods.asScala

    @inline def methods_+=(newMethod: SootMethod) = v.addMethod(newMethod)

    @inline def outerClass = v.getOuterClass

    @inline def outerClass_=(sc: SootClass) = v.setOuterClass(sc)

    @inline def outerClassOpt = ifToOption(v.hasOuterClass, v.getOuterClass)

    @inline def typ = v.getType

    @inline def typ_=(newTyp: RefType) = v.setRefType(newTyp)

    @inline def field(subSignature: String) = v.getField(subSignature)

    @inline def fieldOpt(subSignature: String) = Option(v.getFieldUnsafe(subSignature))

    @inline def field(name: String, typ: Type) = v.getField(name, typ)

    @inline def fieldOpt(name: String, typ: Type) = Option(v.getFieldUnsafe(name, typ))

    @inline def fieldByName(name: String) = v.getFieldByName(name)

    @inline def fieldByNameOpt(name: String) = Option(v.getFieldByNameUnsafe(name))

    @inline def fieldsByName(name: String) = fields.filter(_.name == name)

    @inline def methodsByName(name: String) = methods.filter(_.name == name)

    @inline def methodByName(name: String) = v.getMethodByName(name)

    @inline def methodByNameOpt(name: String) = Option(v.getMethodByNameUnsafe(name))

    @inline def method(subSignature: String) = v.getMethod(subSignature)

    @inline def methodOpt(subSignature: String) = Option(v.getMethodUnsafe(subSignature))

    @inline def method(subSignature: NumberedString) = v.getMethod(subSignature)

    @inline def methodOpt(subSignature: NumberedString) = Option(v.getMethodUnsafe(subSignature))

    @inline def method(name: String, paramTypes: List[Type]) = v.getMethod(name, paramTypes.asJava)

    @inline def methodOpt(name: String, paramTypes: Seq[Type]) = {
      val paramJava = paramTypes.asJava
      ifToOption(v.declaresMethod(name, paramJava), v.getMethod(name, paramJava))
    }

    @inline def method(name: String, paramTypes: List[Type], retType: Type) = v.getMethod(name, paramTypes.asJava, retType)

    @inline def methodOpt(name: String, paramTypes: List[Type], retType: Type) = Option(v.getMethodUnsafe(name, paramTypes.asJava, retType))

    //Those have good getter APIs, but annoying setters
    @inline def inScene_=(flag: Boolean) = v.setInScene(flag)

    @inline def resolvingLevel_=(lvl: Int) = v.setResolvingLevel(lvl)
  }

  implicit class RichCallGraph(val v: CallGraph) extends AnyVal {
    @inline def callersOf(callee: SootMethod) = v.edgesInto(callee).asScala.map(_.getSrc.method())

    @inline def calleesOf(callSite: SootUnit) = v.edgesOutOf(callSite).asScala.map(_.getTgt.method())

    @inline def calleesFrom(method: SootMethod) = v.edgesOutOf(method).asScala.map(_.getTgt.method())
  }

  implicit class RichRefType(val v: RefType) extends AnyVal {
    @inline def anySubType = v.getAnySubType

    @inline def anySubType_=(ast: AnySubType) = v.setAnySubType(ast)

    @inline def arrayElementType = v.getArrayElementType

    @inline def arrayType = v.getArrayType

    @inline def arrayType_=(at: ArrayType) = v.setArrayType(at)

    @inline def className = v.getClassName

    @inline def className_=(cn: String) = v.setClassName(cn)

    @inline def sootClass = v.getSootClass

    @inline def sootClass_=(sc: SootClass) = v.setSootClass(sc)

    @inline def number = v.getNumber

    @inline def number_=(n: Int) = v.setNumber(n)
  }

  implicit class RichClassMember(val c: ClassMember) extends AnyVal {
    @inline def declaringClass = c.getDeclaringClass

    @inline def modifiers = c.getModifiers

    @inline def modifiers_=(mods: Int) = c.setModifiers(mods)

    @inline def phantom_=(flag: Boolean) = c.setPhantom(flag)
  }

  implicit class RichSootField(val v: SootField) extends AnyVal {
    @inline def name = v.getName

    @inline def name_=(newName: String) = v.setName(newName)

    @inline def declaringClass = v.getDeclaringClass

    @inline def signature = v.getSignature

    @inline def subSignature = v.getSubSignature

    @inline def declaration = v.getDeclaration

    /** @return A field signature that is formatted a bit different than the Soot signature.
      *         It consists of the class' name, followed by '.' and the field name.
      */
    @inline def quasiSignature = v.getDeclaringClass.getName + "." + v.getName

    @inline def typ = v.getType
  }

  implicit class RichSootMethod(val v: SootMethod) extends AnyVal {
    @inline def isClinit = v.getName == "<clinit>"

    @inline def name = v.getName

    @inline def name_=(newName: String) = v.setName(newName)

    @inline def declared = v.isDeclared

    @inline def declared_=(flag: Boolean) = v.setDeclared(true)

    @inline def signature = v.getSignature

    @inline def subSignature = v.getSubSignature

    @inline def body = v.retrieveActiveBody()

    @inline def body_=(body: Body) = v.setActiveBody(body)

    @inline def bodyOpt: Option[Body] = ifToOption(v.hasActiveBody, v.getActiveBody)

    @inline def source = v.getSource

    @inline def source_=(ms: MethodSource) = v.setSource(ms)

    @inline def parameterCount = v.getParameterCount

    @inline def parameterTypes = v.getParameterTypes.asScala

    @inline def parameterTypes_=(newPt: Seq[Type]) = v.setParameterTypes(newPt.asJava)

    @inline def exceptions = v.getExceptions.asScala

    @inline def exceptions_=(newEx: Seq[SootClass]) = v.setExceptions(newEx.asJava)

    @inline def returnType = v.getReturnType

    @inline def returnType_=(typ: Type) = v.setReturnType(typ)

    @inline def declaringClass = v.getDeclaringClass

    @inline def declaringClass_=(sc: SootClass): Unit = {
      v.setDeclaringClass(sc)
      v.setDeclared(true)
    }

    @inline def locals = if (v.hasActiveBody) v.body.getLocals else new HashChain[Local]()

    @inline def units = if (v.hasActiveBody) v.body.getUnits else new HashChain[SootUnit]()

    @inline def statements = if (v.hasActiveBody) v.body.units.asInstanceOf[Chain[Stmt]] else new HashChain[Stmt]()

    @inline def numberedSignature = v.getNumberedSubSignature

    @inline def paramLocals = for (i <- 0 until v.getParameterCount; bod <- v.bodyOpt) yield bod.getParameterLocal(i)
  }

  implicit class RichSootMethodRef(val v: SootMethodRef) extends AnyVal {
    @inline def isClinit = v.getName == "<clinit>"

    @inline def signature = v.getSignature

    @inline def subSignature = v.getSubSignature.getString

    @inline def paramTypes = v.getParameterTypes.asScala
  }

  implicit class RichTrap(val v: Trap) extends AnyVal {
    @inline def beginUnit = v.getBeginUnit

    @inline def beginUnit_=(newU: SootUnit) = v.setBeginUnit(newU)

    @inline def beginStmt = v.getBeginUnit.asInstanceOf[Stmt]

    @inline def endUnit = v.getEndUnit

    @inline def endUnit_=(newU: SootUnit) = v.setEndUnit(newU)

    @inline def endStmt = v.getEndUnit.asInstanceOf[Stmt]

    @inline def exception = v.getException

    @inline def exception_=(sc: SootClass) = v.setException(sc)

    @inline def handlerUnit = v.getHandlerUnit

    @inline def handlerUnit_=(newU: SootUnit) = v.setHandlerUnit(newU)

    @inline def handlerStmt = v.getHandlerUnit.asInstanceOf[Stmt]
  }

  implicit class RichBody(val v: Body) extends AnyVal {
    @inline def units = v.getUnits

    @inline def statements = v.getUnits.asInstanceOf[PatchingChain[Stmt]]

    @inline def locals = v.getLocals

    @inline def method = v.getMethod

    @inline def thisLocal = Try(v.getThisLocal).toOption

    @inline def parameterLocal(i: Int) = v.getParameterLocal(i)

    @inline def traps = v.getTraps

    @inline def parameterLocals = v.getParameterLocals.asScala

    @inline def sources = units.map(it => s"  ${it.toString()} // L${it.lineNumberOpt.getOrElse("0")}").mkString(s"$method {\n", "\n", "\n}")
  }

  object SStmt {
    def unapply(stmt: Stmt): Option[(Option[InvokeExpr], Option[ArrayRef], Option[FieldRef])] =
      Some(stmt.invokeExprOpt, stmt.arrayRefOpt, stmt.fieldRefOpt)
  }

  implicit class RichStmt(val v: Stmt) extends AnyVal {
    @inline def invokeExpr = v.getInvokeExpr

    @inline def invokeExprOpt = ifToOption(v.containsInvokeExpr(), v.getInvokeExpr)

    @inline def arrayRef = v.getArrayRef

    @inline def arrayRefOpt = ifToOption(v.containsArrayRef(), v.getArrayRef)

    @inline def fieldRef = v.getFieldRef

    @inline def fieldRefOpt = ifToOption(v.containsFieldRef(), v.getFieldRef)
  }

  object SIfStmt {
    def unapply(stmt: IfStmt): Option[(Value, Stmt)] = Some(stmt.condition, stmt.target)
  }

  implicit class RichIfStmt(val v: IfStmt) extends AnyVal {
    @inline def condition = v.getCondition

    @inline def target = v.getTarget
  }

  object SBinopExpr {
    def unapply(expr: BinopExpr): Option[(Value, Value)] = Some(expr.left, expr.right)
  }

  implicit class RichBinopExpr(val v: BinopExpr) extends AnyVal {
    @inline def left = v.getOp1

    @inline def right = v.getOp2
  }

  object SEqExpr {
    def unapply(exp: EqExpr): Option[(Value, Value)] = Some(exp.left, exp.right)
  }

  implicit class RichFastHierarchy(val v: FastHierarchy) extends AnyVal {
    @inline def abstractDispatch(sm: SootMethod) = v.resolveAbstractDispatch(sm.getDeclaringClass, sm).asScala.toSet

    @inline def interfaceImplementers(sc: SootClass) = if (sc.isInterface) v.getAllImplementersOfInterface(sc).asScala.toSet else Set[SootClass]()

    @inline def subClassesOf(sc: SootClass) = v.getSubclassesOf(sc).asScala.toSet

    @inline def allSubinterfaces(sc: SootClass) = v.getAllSubinterfaces(sc).asScala.toSet
  }

  implicit class RichScene(val v: Scene) extends AnyVal {

    @inline def applicationClasses = v.getApplicationClasses.asScala

    @inline def classes = v.getClasses.asScala

    @inline def libraryClasses = v.getLibraryClasses.asScala

    @inline def phantomClasses = v.getPhantomClasses.asScala

    @inline def field(fieldSpec: String) = v.getField(fieldSpec)

    @inline def fieldOpt(fieldSpec: String) = ifToOption(v.containsField(fieldSpec), v.getField(fieldSpec))

    @inline def fieldRef(fieldSpec: String) = v.getField(fieldSpec).makeRef()

    @inline def fieldRefOpt(fieldSpec: String) = ifToOption(v.containsField(fieldSpec), v.getField(fieldSpec).makeRef())

    @inline def refType(className: String) = v.getRefType(className)

    @inline def refTypeOpt(className: String) = ifToOption(v.containsType(className), v.getRefType(className))

    @inline def sootClass(className: String) = v.getSootClass(className)

    @inline def sootClassOpt(className: String): Option[SootClass] = Option(v.getSootClassUnsafe(className))

    @inline def method(sig: String) = v.getMethod(sig)

    @inline def methodOpt(sig: String) = ifToOption(v.containsMethod(sig), v.getMethod(sig))

    @inline def methodRef(sig: String) = v.getMethod(sig).makeRef()

    @inline def methodRefOpt(sig: String): Option[SootMethodRef] = ifToOption(v.containsMethod(sig), v.getMethod(sig).makeRef())

    @inline def objectType = v.getObjectType

    @inline def objectClass = v.getObjectType.getSootClass

    @inline def hierarchy = v.getActiveHierarchy

    @inline def hierarchy_=(h: Hierarchy) = v.setActiveHierarchy(h)

    @inline def fastHierarchy = v.getOrMakeFastHierarchy

    @inline def fastHierarchy_=(fh: FastHierarchy) = v.setFastHierarchy(fh)

    @inline def callGraph = v.getCallGraph

    @inline def callGraph_=(cg: CallGraph) = v.setCallGraph(cg)

    @inline def contextNumberer = v.getContextNumberer

    @inline def contextNumberer_=(cn: Numberer[Context]) = v.setContextNumberer(cn)

    @inline def contextSensitiveCallGraph = v.getContextSensitiveCallGraph

    @inline def contextSensitiveCallGraph_=(cscg: ContextSensitiveCallGraph) = v.setContextSensitiveCallGraph(cscg)

    @inline def defaultThrowAnalysis = v.getDefaultThrowAnalysis

    @inline def defaultThrowAnalysis_=(ta: ThrowAnalysis) = v.setDefaultThrowAnalysis(ta)

    @inline def entryPoints = v.getEntryPoints.asScala

    @inline def entryPoints_=(ep: Seq[SootMethod]) = v.setEntryPoints(ep.asJava)

    @inline def mainClass = v.getMainClass

    @inline def mainClass_=(sc: SootClass) = v.setMainClass(sc)

    @inline def mainMethod = v.getMainMethod

    @inline def mainMethod_=(sm: SootMethod): Unit = v.setMainClass(sm.declaringClass)

    @inline def phantomRefs = v.getPhantomRefs

    @inline def phantomRefs_=(flag: Boolean) = v.setPhantomRefs(flag)

    @inline def pkgList = v.getPkgList.asScala

    @inline def pkgList_=(pl: Seq[String]) = v.setPkgList(pl.asJava)

    @inline def pta = v.getPointsToAnalysis

    @inline def pointsToAnalysis = v.getPointsToAnalysis

    @inline def pointsToAnalysis_=(pta: PointsToAnalysis) = v.setPointsToAnalysis(pta)

    @inline def pag = v.getPointsToAnalysis.asInstanceOf[PAG]

    @inline def sootClassPath = v.getSootClassPath

    @inline def sootClassPath_=(scp: String) = v.setSootClassPath(scp)

    @inline def reachableMethods = v.getReachableMethods

    @inline def reachableMethods_=(rm: ReachableMethods) = v.setReachableMethods(rm)

    @inline def sideEffectAnalysis = v.getSideEffectAnalysis

    @inline def sideEffectAnalysis_=(sea: SideEffectAnalysis) = v.setSideEffectAnalysis(sea)

    @inline def reservedNames = v.getReservedNames.asScala.toSet

  }

  implicit class RichChain[E](val v: Chain[E]) extends Iterable[E] {
    def ++=(elems: Seq[E]) = v.addAll(elems.asJava)

    def +=(elem: E) = v.addLast(elem)

    override def iterator: Iterator[E] = v.iterator().asScala
  }

  implicit class RichHost(val v: Host) extends AnyVal {
    @inline def tags = v.getTags.asScala

    @inline def tag(aName: String) = v.getTag(aName)

    @inline def tagOpt(aName: String): Option[Tag] = Option(v.getTag(aName))

    //This is dirty, but I can't think of a much better way without defaulting to `tagOpt(String)`
    //http://failex.blogspot.ca/2013/06/fake-theorems-for-free.html
    @inline def tagOpt[T <: Tag](typ: Class[T]): Option[T] = v.tags.find(t => t.getClass eq typ).map(_.asInstanceOf[T])

    /** Returns -1 if the annotation is missing */
    @inline def lineNumber = v.getJavaSourceStartLineNumber

    @inline def lineNumberOpt: Option[Int] = v.getJavaSourceStartLineNumber match {
      case -1  => None
      case any => Some(any)
    }

  }

  object SVisibilityAnnotationTag {
    def apply(annotations: AnnotationTag*) = {
      val annotationTag = new VisibilityAnnotationTag(0)
      annotations.foreach(annotationTag.addAnnotation)
      annotationTag
    }

    def apply(annotations: Iterable[AnnotationTag]) = {
      val annotationTag = new VisibilityAnnotationTag(0)
      annotations.foreach(annotationTag.addAnnotation)
      annotationTag
    }

    def unapply(vat: VisibilityAnnotationTag) = vat.annotations
  }

  implicit class RichVisibilityAnnotationTag(val v: VisibilityAnnotationTag) extends AnyVal {
    @inline def annotations = v.getAnnotations.asScala
  }

  object SAnnotationTag {
    def apply(name: String, elements: Seq[AnnotationElem] = Seq()) = new AnnotationTag(name, elements.asJava)

    /** @param at the `AnnotationTag`
      * @return a tuple with (annotation name, information, elements)
      */
    def unapply(at: AnnotationTag) = Some(at.name, at.info, at.elements)
  }

  object SAnnotationStringElem {
    def apply(name: String, value: String) = new AnnotationStringElem(value, 's', name)
  }

  implicit class RichAnnotationTag(val v: AnnotationTag) extends AnyVal {
    @inline def elements = v.getElems.asScala

    @inline def info = v.getInfo

    @inline def name = v.getName
  }

  object SAnnotationElem {
    def unapply(ae: AnnotationElem) = Some(ae.name, ae.kind)
  }

  implicit class RichAnnotationElement(val v: AnnotationElem) extends AnyVal {
    @inline def kind = v.getKind

    @inline def name = v.getName
  }

  implicit class RichValue(private val v: Value) extends AnyVal {
    @inline def useBoxes = v.getUseBoxes.asScala.toSeq
  }

  object SInvokeExpr {

    /** @param expr the expression
      * @return a tuple with (an `Option` to the base variable, the sequence of arguments, the target method)
      */
    def unapply(expr: InvokeExpr) = expr match {
      case SStaticInvokeExpr(args, method)         => Some(None, args, method)
      case SInstanceInvokeExpr(base, args, method) => Some(Some(base), args, method)
      case _                                       => throw new RuntimeException("Unhandled invoke expression type")
    }
  }

  implicit class RichInvokeExpr(val v: InvokeExpr) extends AnyVal {
    @inline def args = v.getArgs.asScala

    @inline def arg(index: Int) = v.getArg(index)

    @inline def argCount = v.getArgCount

    @inline def method = v.getMethod

    //In some modes, this getMethod throw an exception. In others, it merely returns false.
    //So we handle both those cases with Try and Option together. The actual exception gets lost
    //in the process, but that's probably not a big deal
    @inline def methodOpt = Try(Option(v.getMethod)).getOrElse(None)

    @inline def methodRef = v.getMethodRef

    @inline def returnType = v.getType
  }

  object SStaticInvokeExpr {
    def apply(args: Seq[Value], target: SootMethod): StaticInvokeExpr = Jimple.v.newStaticInvokeExpr(target.makeRef(), args.asJava)

    /** @param expr the expression
      * @return a tuple with (the sequence of arguments, the target method)
      */
    def unapply(expr: StaticInvokeExpr) = Some(expr.args, expr.method)
  }

  object SInstanceInvokeExpr {
    def apply(base: Local, args: Seq[Value], target: SootMethod): InstanceInvokeExpr = target.declaringClass match {
      case interface if interface.isInterface => Jimple.v.newInterfaceInvokeExpr(base, target.makeRef(), args.asJava)
      case _                                  => Jimple.v.newVirtualInvokeExpr(base, target.makeRef(), args.asJava)
    }

    /** @param expr the expression
      * @return a tuple with (the base variable, the sequence of arguments, the target method)
      */
    def unapply(expr: InstanceInvokeExpr) = Some(expr.base, expr.args, expr.method)
  }

  implicit class RichInstanceInvokeExpr(val v: InstanceInvokeExpr) extends AnyVal {
    @inline def base = v.getBase
  }

  object SLocal {
    def unapply(l: Local): Option[(String, Type)] = Some(l.name, l.getType)
  }

  implicit class RichLocal(val v: Local) extends AnyVal {
    @inline def name = v.getName

    @inline def name_=(n: String) = v.setName(n)
  }

  object SArrayRef {
    def unapply(ar: ArrayRef): Option[(Value, Value)] = Some(ar.base, ar.index)
  }

  implicit class RichArrayRef(val v: ArrayRef) extends AnyVal {
    @inline def base = v.getBase

    @inline def baseBox = v.getBaseBox

    @inline def index = v.getIndex

    @inline def indexBox = v.getIndexBox
  }

  object SFieldRef {
    def apply(sf: SootField) = sf.makeRef()

    def unapply(fr: FieldRef) = Some(fr.getField)
  }

  implicit class RichFieldRef(val v: FieldRef) extends AnyVal {
    @inline def field = v.getField

    @inline def fieldRef = v.getFieldRef

    @inline def fieldRef_=(sfr: SootFieldRef) = v.setFieldRef(sfr)
  }

  object SStaticFieldRef {
    def apply(sf: SootField): SootFieldRef = sf.makeRef()

    def unapply(fr: StaticFieldRef): Option[SootField] = Some(fr.getField)
  }

  object SCastExpr {
    def unapply(ce: CastExpr): Option[(Value, Type)] = Some(ce.op, ce.castType)
  }

  implicit class RichCastExpr(val v: CastExpr) extends AnyVal {
    @inline def op = v.getOp

    @inline def op_=(newOp: Value) = v.setOp(newOp)

    @inline def opBox = v.getOpBox

    @inline def castType = v.getCastType

    @inline def castType_=(newCt: Type) = v.setCastType(newCt)
  }

  object SDefinitionStmt {

    /** Extractor that gives a tuple with the left op and the right op (in that order)
      *
      * @param ds the assign statement
      * @return (left op, right op)
      */
    def unapply(ds: DefinitionStmt): Option[(Value, Value)] = Some(ds.leftOp, ds.rightOp)
  }

  implicit class RichDefinitionStmt(val v: DefinitionStmt) extends AnyVal {
    @inline def leftOp = v.getLeftOp

    @inline def leftOpBox = v.getLeftOpBox

    @inline def rightObBox = v.getRightOpBox

    @inline def rightOp = v.getRightOp
  }

  object SIdentityStmt {

    /** Extractor that gives a tuple with the left op and the right op (in that order)
      *
      * @param is the identity statement
      * @return (left op, right op)
      */
    def unapply(is: IdentityStmt): Option[(Value, Value)] = Some(is.leftOp, is.rightOp)
  }

  object SInstanceFieldRef {
    def unapply(as: InstanceFieldRef) = Some(as.getBase, as.field)
  }

  object SNewExpr {
    def unapply(as: NewExpr) = Some(as.getBaseType)
  }

  object SAssignStmt {
    def apply(left: Value, right: Value): AssignStmt = Jimple.v.newAssignStmt(left, right)

    /** Extractor that gives a tuple with the left op and the right op (in that order)
      *
      * @param as the assign statement
      * @return (left op, right op)
      */
    def unapply(as: AssignStmt) = Some(as.leftOp, as.rightOp)
  }

  implicit class RichAssignStmt(val v: AssignStmt) extends AnyVal {
    @inline def rightOp_=(newRo: Value) = v.setRightOp(newRo)

    @inline def leftOp_=(newLo: Value) = v.setLeftOp(newLo)
  }

  object SReturnStmt {
    def unapply(rs: ReturnStmt): Option[Value] = Some(rs.op)
  }

  implicit class RichReturnStmt(val v: ReturnStmt) extends AnyVal {
    @inline def op = v.getOp

    @inline def op_=(newOp: Value) = v.setOp(newOp)
  }

  implicit class RichOptions(val v: Options) extends AnyVal {
    @inline def classPath = v.soot_classpath()

    @inline def classPath_=(newCp: String) = v.set_soot_classpath(newCp)

    @inline def classPath_=(newCp: Seq[Path]) = v.set_soot_classpath(newCp.mkString(File.pathSeparator))

    @inline def processPath: Seq[String] = v.process_dir().asScala.toSeq

    @inline def processPath_=(newPp: Seq[String]) = v.set_process_dir(newPp.asJava)

    @inline def allowPhantomRefs = v.allow_phantom_refs()

    @inline def allowPhantomRefs_=(apr: Boolean): Unit = v.set_allow_phantom_refs(apr)

    @inline def androidJars = v.android_jars()

    @inline def androidJars_=(aj: String): Unit = v.set_android_jars(aj)

    @inline def inAppMode = v.app()

    @inline def inAppMode_=(setting: Boolean) = v.set_app(setting)

    @inline def computeAstMetrics = v.ast_metrics()

    @inline def computeAstMetrics_=(setting: Boolean): Unit = v.set_ast_metrics(setting)

    @inline def checkInitThrowAnalysis = v.check_init_throw_analysis()

    @inline def checkInitThrowAnalysis_=(setting: Int): Unit = v.set_check_init_throw_analysis(setting)

    @inline def debug_=(setting: Boolean): Unit = v.set_debug(setting)

    @inline def debugResolver = v.debug_resolver()

    @inline def debugResolver_=(setting: Boolean): Unit = v.set_debug_resolver(setting)

    @inline def dumpBody = v.dump_body().asScala.toSeq

    @inline def dumpBody_=(setting: Seq[String]) = v.set_dump_body(setting.asJava)

    @inline def dumpCfg = v.dump_cfg().asScala.toSeq

    @inline def dumpCfg_=(setting: Seq[String]) = v.set_dump_cfg(setting.asJava)

    @inline def dynamicClass = v.dynamic_class().asScala.toSeq

    @inline def dynamicClass_=(setting: Seq[String]) = v.set_dynamic_class(setting.asJava)

    @inline def dynamicDir = v.dynamic_dir().asScala.toSeq

    @inline def dynamicDir_=(setting: Seq[String]) = v.set_dynamic_dir(setting.asJava)

    @inline def dynamicPackage = v.dynamic_package().asScala.toSeq

    @inline def dynamicPackage_=(setting: Seq[String]) = v.set_dynamic_package(setting.asJava)

    @inline def excludes = v.exclude().asScala.toSeq

    @inline def excludes_=(setting: Seq[String]) = v.set_exclude(setting.asJava)

    @inline def forceAndroidJar = v.force_android_jar()

    @inline def forceAndroidJar_=(setting: String) = v.set_force_android_jar(setting)

    @inline def fullResolver = v.full_resolver()

    @inline def fullResolver_=(setting: Boolean): Unit = v.set_full_resolver(setting)

    @inline def phaseHelp(phase: String) = v.getPhaseHelp(phase)

    @inline def phaseHelp = v.phase_help().asScala.toSeq

    @inline def phaseHelp_=(help: Seq[String]): Unit = v.set_phase_help(help.asJava)

    @inline def phaseList = v.phase_list()

    @inline def phaseList_=(setting: Boolean) = v.set_phase_list(setting)

    @inline def gzip_=(setting: Boolean) = v.set_gzip(setting)

    @inline def help_=(setting: Boolean) = v.set_help(setting)

    @inline def ignoreResolutionErrors = v.ignore_resolution_errors()

    @inline def ignoreResolutionErrors_=(setting: Boolean) = v.set_ignore_resolution_errors(setting)

    @inline def includes = v.include().asScala.toSeq

    @inline def includes_=(setting: Seq[String]) = v.set_include(setting.asJava)

    @inline def includeAll = v.include_all()

    @inline def includeAll_=(setting: Boolean) = v.set_include_all(setting)

    @inline def interactiveMode = v.interactive_mode()

    @inline def interactiveMode_=(setting: Boolean) = v.set_interactive_mode(setting)

    @inline def j2me_=(setting: Boolean) = v.set_j2me(setting)

    @inline def keepLineNumber = v.keep_line_number()

    @inline def keepLineNumber_=(keep: Boolean) = v.set_keep_line_number(keep)

    @inline def keepOffset = v.keep_offset()

    @inline def keepOffset_=(setting: Boolean) = v.set_keep_offset(setting)

    @inline def mainClass = v.main_class()

    @inline def mainClass_=(mc: String) = v.set_main_class(mc)

    @inline def noBodiesForExcluded = v.no_bodies_for_excluded()

    @inline def noBodiesForExcluded_=(setting: Boolean) = v.set_no_bodies_for_excluded(setting)

    @inline def noOutputInnerClassesAttribute = v.no_output_inner_classes_attribute()

    @inline def noOutputInnerClassesAttribute_=(setting: Boolean) = v.set_no_output_inner_classes_attribute(setting)

    @inline def noOutputSourceFileAttribute = v.no_output_source_file_attribute()

    @inline def noOutputSourceFileAttribute_=(setting: Boolean) = v.set_no_output_source_file_attribute(setting)

    @inline def ooat_=(setting: Boolean) = v.set_oaat(setting)

    @inline def omitExceptingUnitEdges = v.omit_excepting_unit_edges()

    @inline def omitExceptingUnitEdges_=(setting: Boolean) = v.set_omit_excepting_unit_edges(setting)

    @inline def onTheFly = v.on_the_fly()

    @inline def onTheFly_=(setting: Boolean) = v.set_on_the_fly(setting)

    @inline def outputDir = v.output_dir()

    @inline def outputDir_=(setting: String) = v.set_output_dir(setting)

    @inline def outputDir_=(setting: Path) =
      v.set_output_dir(setting.toAbsolutePath.toString) //toAbsolutePath because the directory may not exist yet

    @inline def outputFormat = v.output_format()

    @inline def outputFormat_=(setting: Int) = v.set_output_format(setting)

    @inline def outputJar = v.output_jar()

    @inline def outputJar_=(setting: Boolean) = v.set_output_jar(setting)

    @inline def prependClassPath: Boolean = v.prepend_classpath()

    @inline def prependClassPath_=(setting: Boolean) = v.set_prepend_classpath(setting)

    @inline def wholeProgram = v.whole_program()

    @inline def wholeProgram_=(setting: Boolean) = v.set_whole_program(setting)

    @inline def srcPrec = v.src_prec()

    @inline def srcPrec_=(setting: Int) = v.set_src_prec(setting)

    @inline def time_=(setting: Boolean) = v.set_time(setting)

    @inline def noWriteOutBodyReleasing = v.no_writeout_body_releasing()

    @inline def noWriteOutBodyReleasing_=(setting: Boolean) = v.set_no_writeout_body_releasing(setting)

  }

  object SStringConstant {
    def apply(s: String) = StringConstant.v(s)
  }

  object SIntConstant {
    def apply(i: Int) = IntConstant.v(i)
  }

  object SLongConstant {
    def apply(l: Long) = LongConstant.v(l)
  }

  object SDoubleConstant {
    def apply(d: Double) = DoubleConstant.v(d)
  }

  object SFloatConstant {
    def apply(d: Float) = FloatConstant.v(d)
  }
}
