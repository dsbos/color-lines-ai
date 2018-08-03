package com.us.dsb.explore.types.typeclasses.interpreter

import org.scalatest.FunSpec


/**
  * Interpreter-pattern exploration with just independent interpreters (e.g.,
  * no common `fold` method to help keep client code synchronized with set of
  * subclasses.)
  */
class CepInterpreterSketchTest extends FunSpec {

  /*
  Basic idea, iso(?)morphism with straight method calls.

  (currenty ignoring passing input and output data0

  #1: Base form--tree of methods and called methods, executed and immediately
  def processMessage = {
    processPreEventThings
    processEventSet
    processPostEventThings
  }

  #2:  Tree of data nodes representing computation, build from ~isomorphic
      tree of data-assembly expressions/variable; processing of computation is
      not limited to just execution--it can be analyzed, symbolically executed,
      etc.; execution needs execution-interpretation methods (per type of thing
      in tree, not per instance)

  trait BaseProcessing { ... }  // higher levels pass full data

  class class CompoundProcessing(processingSteps: BaseProcessing*)
    extends BaseProcessing

  val messageProcessing =
    CompoundProcessing(    // later--maybe sequential vs. parallelizable
      preEventProcessing
      eventSetProcessing
      postEventProcessing
    )

  def interpretCompoundProcessing(nameThis: CompoundProcessing) = {
   nameThis.processingSteps.foreach(step => interpretProcessing(step))
  }

  def interpretProcessing(nameThis: BaseProcessing) = {
    match nameThis {
      case nt: CompoundProcessing => interpretCompoundProcessing(nt)
      case nt: PerEventProcessing => interpretPerEventTypeProcession(nt)
      ...
    }
  }


  */

  // Simulated CEP data (messages; thing events, state, definition; etc.)

  case class ThingDefinition(someField: String)

  sealed trait Event

  case class StartEvent() extends Event

  //???? what about carried definition?; common vs. interpreter-specific data; event classes vs. just event kinds
  //???(thing definition specification (not necessarily whole definition)
  case class CreationEvent(definition: ThingDefinition) extends Event

  sealed trait StateField
  object StateFields {
    // (Lower camel case because represents field in class.
    case object miscData extends StateField
  }

  // TODO:  Revisit:  Possibly use ~LabelOp instead of label parameter on every
  // constructor
  //  - +: to avoid clutter in constructor calls
  //  - -: makes graph less regular (complicates graph manipulations)
  //       - but regularization step could replace LabelOp around Op with
  //         copy of Op with label set
  //  - ~: can use alternative constructors to reduce call clutter (probably)

  sealed trait ProcessingOp {
    def label: String
  }

  case class SequenceOp(label: String, steps: ProcessingOp*)
      extends ProcessingOp

  /** ... has no ProcessingOp descendants (can have other descendants, e.g., expr. trees)) */
  sealed trait PrimitiveOp

  sealed trait KnownPrimitive
  object KnownPrimitive {

    case object Creation_ValidateEvent   extends KnownPrimitive
    case object Creation_CopyDefinition  extends KnownPrimitive  //???? finding all updaters of definition would be nice
    case object Creation_ModifyMiscState extends KnownPrimitive
    //
    case object DummyClearMiscData                 extends KnownPrimitive
    case class DummySuffixMiscData(suffix: String) extends KnownPrimitive
    case class TempStringNamedPrimitive(name: String) extends KnownPrimitive
  }
  import KnownPrimitive._

  case class KnownPrimitiveOp(label: String, kind: KnownPrimitive)
      extends ProcessingOp

  /* ... semi-generic: different target fields, but always just clears */
  case class SetStateFieldEmpty(label:String, field: StateField)
      extends ProcessingOp


  // ... (life-cycle state field is domain-specific, but is very significant)
  case class SetLifecycleOp(label: String, lifecycleState: LifecycleState)
      extends ProcessingOp
  type LifecycleState = String
  case class PerLifecyleStateOp(label: String, map: (LifecycleState, ProcessingOp)*)
      extends ProcessingOp

  case class PerEventKindOp(label: String, map: (Class[_ <: Event], ProcessingOp)*)
      extends ProcessingOp

  /**
    * Primitive operation that doesn't need case in interpreters but supports
    * only fixed set of interpretations.
    * For convenience (putting execution code in instantiation, vs. separately
    * in evaluation interpretation method (and creating new KnownPrimitive
    * subclass).  Is temporary/interim HACK:  Supports only two interpretations
    * (adding interpretation would require updating all instantiations), so
    * instantiations should be converted to KnownPrimitiveOp instantiations
    * with new KnownPrimitive cases.
    */
  case class HackAdHocPrimitiveOp(override val label: String,
                                  formatString: String,
                                  executionFn: InAndOutData => InAndOutData)
      extends ProcessingOp



  //////////
  // Interpreter 1:  Evaluation:

  case class InAndOutData(eventKind: Class[_ <: Event],
                          event: Event,
                          lifecycleState: LifecycleState,
                          miscData: String,
                          definition: Option[ThingDefinition])

  // ?? Scala:  Type classes?

  def evaluate(op: ProcessingOp, data: InAndOutData): InAndOutData = {

    // ?? Scala:  Implicit parameter for repeated InAndOutData?

    def evaluateKnownPrimitiveOp(op: KnownPrimitiveOp, data: InAndOutData): InAndOutData = {
      val label = op.label
      op.kind match {
        case DummySuffixMiscData(suffix) =>
          data.copy(miscData = data.miscData + suffix)
        case kind @ Creation_ValidateEvent => data
        case kind @ Creation_CopyDefinition =>
          // ?? Scala:  Can we eliminate(/move/hide) cast(s)? (Maybe via method on Creation_xxx object(s)?
          data.copy(definition = Some(data.event.asInstanceOf[CreationEvent].definition))
        case kind @ Creation_ModifyMiscState => data
        case TempStringNamedPrimitive(kind) =>
          data  // imagine processing according to string (e.g., match/case)
        case kind: KnownPrimitive =>
          "" + ??? + s"- '$label': UNDIFFERENTIATED known primitive op (enumerated): (${kind})"
          ???
      }
    }

    def evaluateSetStateFieldEmpty(op: SetStateFieldEmpty, data: InAndOutData): InAndOutData = {
      val label = op.label
      op.field match {
        case miscData => data.copy(miscData = "<EMPTIED>")
      }
    }

    val label = op.label
    System.err.println(s"(+'$label')")
    val value: InAndOutData =
      op match {
        case op: KnownPrimitiveOp   => evaluateKnownPrimitiveOp(op, data)
        case op: SetStateFieldEmpty => evaluateSetStateFieldEmpty(op, data)
        case HackAdHocPrimitiveOp(_, _, execFn) => execFn(data)
        case SetLifecycleOp(_, lifecycleState) =>
          data.copy(lifecycleState = lifecycleState)
        case SequenceOp(_, steps @ _*) =>
          steps.foldLeft(data)((data, step) => evaluate(step, data))
        case PerEventKindOp(_, map @ _*) =>
          {
            for (proc <- map.toMap.get(data.eventKind)) yield {
              evaluate(proc, data)
            }
          }.getOrElse(???)
        case PerLifecyleStateOp(_, map @ _*) =>
          locally {
            for (proc <- map.toMap.get(data.lifecycleState)) yield {
              evaluate(proc, data)
            }
          }.getOrElse(???)
      }
    System.err.println(s"(-'$label'): value = " + value)
    value
  }

  //////////
  // Interpreter 2:  Formatting/rendering specification:

  //????? doesn't address non-tree nature of graph (reconvergence)
  def format(op: ProcessingOp): String = {

    def formatKnownPrimitiveOp(indentation: String, op: KnownPrimitiveOp): String = {
      val label = op.label
      op.kind match {
        case kind @ Creation_ValidateEvent =>
          indentation + s"- '$label': known validate-creation-event op (${kind})"

        case TempStringNamedPrimitive(kind) =>
          indentation + s"- '$label': known primitive op, string-named: '${kind}'"
        case kind: KnownPrimitive =>
          indentation + s"- '$label': known primitive op, undifferentiated in formatting: ${kind}"
      }
    }
    def formatSetStateFieldEmpty(indentation: String, op: SetStateFieldEmpty): String = {
      val label = op.label
      op.field match {
        case miscData =>
          indentation + s"- '$label': set state field $miscData to empty"
      }
    }

    def formatSub(indentation: String, op: ProcessingOp): String = {
      op match {
        case proc: KnownPrimitiveOp =>
          formatKnownPrimitiveOp(indentation, proc)
        case HackAdHocPrimitiveOp(label, description, fn) =>
          indentation + s"- '$label': ad-hoc operation: '$description'"
        case proc: SetStateFieldEmpty =>
          formatSetStateFieldEmpty(indentation, proc)
        case SetLifecycleOp(label, value) =>
          indentation + s"- '$label': set lifecycleState to '$value'"
        case SequenceOp(label, steps @ _*) =>
          List(
            indentation + s"- '$label': sequence {",
            steps.map(p => formatSub("  " + indentation, p)).mkString("\n"),
            indentation + s"- } /* sequence '$label'  */ )"
          ).mkString("\n")
        case PerEventKindOp(label, map @ _*) =>
          List(
            indentation + s"- '$label': per event kind: {",
            map.map(pair => {
              indentation + " *" + pair._1.toString +
              "\n" +
              formatSub("  " + indentation, pair._2)
            }).mkString("\n"),
            indentation + s"- } /* per event kind '$label' */"
          ).mkString("\n")
        case PerLifecyleStateOp(label, map @ _*) =>
          List(
            indentation + s"- '$label': per lifecycle state: {",
            map.map(pair => {
              indentation + " *" + pair._1.toString +
              "\n" +
              formatSub("  " + indentation, pair._2)
            }).mkString("\n"),
            indentation + s"- } /* per lifecycle state '$label' */"
          ).mkString("\n")
      }
    }
    formatSub("", op)
  }


  val tryingGraph = {
    val creationEventProcessing =
      SequenceOp(
        "creationEventProcessing",
        KnownPrimitiveOp("", Creation_ValidateEvent),
        KnownPrimitiveOp("", Creation_CopyDefinition),
        SetLifecycleOp("", "draft"),
        KnownPrimitiveOp("", Creation_ModifyMiscState)
      )


    val startEventProcessing = {
      val badStartProcessing =
        SequenceOp("???", SetStateFieldEmpty("", StateFields.miscData),
        KnownPrimitiveOp("badStartProcessing", TempStringNamedPrimitive("StartEvent: RejectedStartEvent")))
      PerLifecyleStateOp(
        "startEventProcessing",
        ("draft",
            SequenceOp(
              "...",
              KnownPrimitiveOp("(good start.1)", TempStringNamedPrimitive("StartEvent processing")),
              SetLifecycleOp("(good start.2)", "ready"),
              KnownPrimitiveOp("(good start.3 - suffix ...)",
                                       DummySuffixMiscData("_suffix2")))
        ),
        ("ready", badStartProcessing),
        ("inTransit", badStartProcessing)
      )
    }

    val preEventProcessing =
      KnownPrimitiveOp("preEventProcessing", TempStringNamedPrimitive("pre-event processing"))

    val eventProcessing  =
      PerEventKindOp(
        "eventProcessing",
        // ?? Scala:  Can we tie types together to avoid needing casting somewhere?
        (classOf[CreationEvent], creationEventProcessing),
        (classOf[StartEvent], startEventProcessing)
      )

    val postEventProcessing =
      KnownPrimitiveOp("postEventProcessing", TempStringNamedPrimitive("post-event processing"))

    SequenceOp(
      "messageProcessing",
      HackAdHocPrimitiveOp("ad-hoc op try", "Print \"Ad-hoc test\"",
                           data => {println("Ad-hoc test"); data}),
      preEventProcessing,
      eventProcessing,
      postEventProcessing
    )
  }

  val basicGraphs =
    List(
      SequenceOp(
        "someLabel",
        KnownPrimitiveOp("...", TempStringNamedPrimitive("Primitive step 1")),
        KnownPrimitiveOp("...", TempStringNamedPrimitive("Primitive step 1")))
    )


  def runInterpretations(label: String,
                         op: ProcessingOp,
                         data: InAndOutData
                        ): InAndOutData = {
    System.err.println()
    System.err.println(s"$label = " + op)

    System.err.println(s"format($label):")
    System.err.println(s"----------:")
    System.err.println(s"" + format(op))
    System.err.println(s":----------")

    System.err.println(s"evaluate($label, $data):")
    System.err.println(s"==========:")
    val result = evaluate(op, data)
    System.err.println(s"= = = = = :")
    System.err.println(result)
    System.err.println(s":==========")
    result
  }

  locally {
    val creation = CreationEvent(ThingDefinition("someField's value"))
    val data1 = InAndOutData(classOf[CreationEvent], creation, "preDraft", "<initial>", None)
    val data2 = runInterpretations("tryingGraph", tryingGraph, data1)

    val data3 = data2.copy(eventKind = classOf[StartEvent], event = null/*??????*/)
    val data4 = runInterpretations("tryingGraph", tryingGraph, data3)

    val data5 = data4.copy(eventKind = classOf[StartEvent], event = null/*??????*/)
    val data6 = runInterpretations("tryingGraph", tryingGraph, data5)

    /*
    List(
      InAndOutData(classOf[StartEvent], "draft"),
      InAndOutData(classOf[StartEvent], "ready")
    ).foreach(data => {
      runInterpretations("tryingGraph", tryingGraph, data)
    })
    */
  }

  //????basicGraphs.zipWithIndex.foreach({case(x, y) =>  runInterpretations("#" + y, x, null)})


  it(""){

  }
}
