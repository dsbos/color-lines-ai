package com.us.dsb.explore.types.typeclasses.interpreter

import org.scalatest.FunSpec


/**
  * Interpreter-pattern exploration with just independent interpreters (e.g.,
  * no common `fold` method to help keep client code synchronized with set of
  * subclasses.)
  */
class CepInterpreterSketchTest extends FunSpec {

  /*

  (currenty ignoring passing input and output data0

  #1: Base form--tree of methods and called methods, executed and immediately
  def processMessage = {
    processPreEventThings
    processEventSet
    processPostEventThings
  }

  (#1.5: --computation function built from isomorphic tree of methods creating
           higher-level function from lower-level functions; _deferred_ execution,
           but still direct and opaque)
    val processMessageFn =
      processPreEventThingsFn
        andThen processEventSetFn
        andThen processPostEventThingsFn
  )

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

  // Is following "interpret..." execution, or is it general traversal used by
  // execution plus as well as any other interpretation/processing of the
  // data tree?:
  // (note that match/case-like construct is much different for execution
  // (one branch only  vs. some operations (e.g., printing--all branches);
  // (well, only one branch fully executes, though others could still be
  // traversed as no-ops)

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


  def processEventSet(...) = {
    ... match {
      case Set(singleEvent) =>
         processEvent(singleEvent)
      case Set(creation: ShipmentCreation, start: ShipmentStart) =>
         processEvent(creation)
         processEvent(start)
      case _ =>
         reportError "unsupported input event combination"
    }
  }


  def processEvent(event: Event) = {
    event match {
      case creation: ShipmentCreation =>
        ...
      case start: ShipmentStart =>
        ...
      case geoUpd: GeolocationUpdate =>
        processGeolocationUpdate(geoUpd)
      case delLocArr: DeliveryLocationArrival =>
        processDeliveryLocationArrival(delLocArr)
      ...
    }
  }

  val eventProcessing =
    PerEventTypeProcessing(  // "per--event-type processing"
      List(
        (ShipmentCreation.type, shipmentStartProcessing),
        (GeolocationUpdate.type, geolocationUpdateProcessing),
        (DeliveryLocationArrival.type, deliveryLocationArrivalProcessing),
        ...
      )

  def interpretPerEventTypeProcessing(nameThis: CompoundProcessing) = {
    get event type from input data,
    look up by type to and get corresponding BaseProcessinng subclass instance,
    interpretProcessing(>looked-up BaseProcessing>)
  }


  val geolocationUpdateProcessing =
    CompoundProcessing(
      trustedGeolocationProcessing,
      ...
    )

  val deliveryLocationArrivalProcessing =
    CompoundProcessing(
      untrustedGeolocationProcessing,
      otherDeliveryLocationArrivalProcessing
      ...
    )

  val trustedGeolocationProcessing = GeolocationProcessing(..., xxx)
  val untrustedGeolocationProcessing = GeolocationProcessing(..., yyy)

  case class PrimitiveProcessing extends BaseProcessing
  case class OtherDeliveryLocationArrivalProcession extends PrimitiveProcessing  // except can't extend like that


  val otherDeliveryLocationArrivalProcessiong =
    OtherDeliveryLocationArrivalProcession

  interpretOtherDeliveryLocationArrivalProcessing() = {
    // "primitive" in processing node/computation tree--
    // specific interpreter knows its interpretation of OtherDeliveryLocationArrivalProcessing;
    // (possibly we could have PrimitiveProcessing define an abstract method
    // or abstract variable for a function for doing the "main" interpretation--
    // CEP execution

  }



  Processing a message devolves to:
  - processing anything that needs to be processed before the event(s) are
  - processing the set of event(s) in the message
  - processing anything that needs to be processed after the event(s) are
  */

  sealed trait Event
  case class StartEvent() extends Event
  case class CreationEvent() extends Event


  sealed trait ProcessingOp {
    def label: String
  }

  case class SequenceOp(label: String, steps: ProcessingOp*)
      extends ProcessingOp

  case class PerEventKindOp(label: String, map: (Class[_ <: Event], ProcessingOp)*)
      extends ProcessingOp

  type LifecycleState = String
  case class PerLifecyleStateOp(label: String, map: (LifecycleState, ProcessingOp)*)
      extends ProcessingOp

  case class SetLifecycleOp(label: String, lifecycleState: LifecycleState)
      extends ProcessingOp


  sealed trait KnownPrimitive
  case object ProcessCreation                    extends KnownPrimitive
  case object DummyClearMiscData                 extends KnownPrimitive
  case class DummySuffixMiscData(suffix: String) extends KnownPrimitive
  case class StringNamedPrimitive(name: String)  extends KnownPrimitive

  case class KnownPrimitiveOp(label: String, kind: KnownPrimitive)
      extends ProcessingOp

  case class NamedPrimitiveOpxx(label: String, name: String)
      extends ProcessingOp

  //////////
  // Interpreter 1:  Evaluation:

  case class InAndOutData(eventKind: Class[_ <: Event],
                          lifecycleState: LifecycleState,
                          miscData: String)

  def evaluate(op: ProcessingOp, data: InAndOutData): InAndOutData = {

    def evaluateKnownPrimitiveOp(op: KnownPrimitiveOp, data: InAndOutData): InAndOutData = {
      val label = op.label
      op.kind match {
        case DummySuffixMiscData(suffix) =>
          data.copy(miscData = data.miscData + suffix)
        case kind @ ProcessCreation =>
          data  // imagine processing a creation event
        case StringNamedPrimitive(kind) =>
          data  // imagine processing according to string (e.g., match/case)
        case kind: KnownPrimitive =>
          "" + ??? + s"- '$label': UNDIFFERENTIATED known primitive op (enumerated): (${kind})"
          ???

      }
    }

    val label = op.label
    System.err.println(s"(+$label)")
    val value: InAndOutData =
      op match {
        case proc: KnownPrimitiveOp =>
          evaluateKnownPrimitiveOp(proc, data)
        case SetLifecycleOp(_, lifecycleState) =>
          data.copy(lifecycleState = lifecycleState)
        case SequenceOp(_, steps @ _*) =>
          var dataN = data
          steps.foreach(proc => dataN = evaluate(proc, dataN))
          dataN
        case PerEventKindOp(_, map @ _*) =>
          locally {
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
    System.err.println(s"(-$label): value = " + value)
    value
  }

  //////////
  // Interpreter 2:  Formatting/rendering specification:

  //????? doesn't address non-tree nature of graph (reconvergence)
  def format(op: ProcessingOp): String = {

    def KnownPrimitiveOp(indentation: String, op: KnownPrimitiveOp): String = {
      val label = op.label
      op.kind match {
        case kind @ ProcessCreation =>
          indentation + s"- '$label': known process-creation-event primitive op (${kind})"
        case StringNamedPrimitive(kind) =>
          indentation + s"- '$label': known primitive op, string-named: '${kind}'"
        case kind: KnownPrimitive =>
          indentation + s"- '$label': UNDIFFERENTIATED known primitive op (enumerated): (${kind})"
      }
    }

    def formatSub(indentation: String, op: ProcessingOp): String = {
      op match {
        case proc: KnownPrimitiveOp =>
          KnownPrimitiveOp(indentation, proc)
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
        KnownPrimitiveOp("creationEventProcessing", ProcessCreation),
        SetLifecycleOp("...", "draft")
      )


    val startEventProcessing = {
      val badStartProcessing =
        KnownPrimitiveOp("badStartProcessing", StringNamedPrimitive("StartEvent: RejectedStartEvent"))
      PerLifecyleStateOp(
        "startEventProcessing",
        ("draft",
            SequenceOp(
              "...",
              KnownPrimitiveOp("(good start.1)", StringNamedPrimitive("StartEvent processing")),
              SetLifecycleOp("(good start.2)", "ready"),
              KnownPrimitiveOp("(good start.3 - suffix ...)",
                                       DummySuffixMiscData("_suffix2")))
        ),
        ("ready", badStartProcessing),
        ("inTransit", badStartProcessing)
      )
    }

    val preEventProcessing =
      KnownPrimitiveOp("preEventProcessing", StringNamedPrimitive("pre-event processing"))

    val eventProcessing  =
      PerEventKindOp(
        "eventProcessing",
        (classOf[CreationEvent], creationEventProcessing),
        (classOf[StartEvent], startEventProcessing)
      )

    val postEventProcessing =
      KnownPrimitiveOp("postEventProcessing", StringNamedPrimitive("post-event processing"))

    SequenceOp(
      "messageProcessing",
      preEventProcessing,
      eventProcessing,
      postEventProcessing
    )
  }

  val basicGraphs =
    List(
      SequenceOp(
        "someLabel",
        KnownPrimitiveOp("...", StringNamedPrimitive("Primitive step 1")),
        KnownPrimitiveOp("...", StringNamedPrimitive("Primitive step 1")))
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
    val data1 = InAndOutData(classOf[StartEvent], "draft", "<initial>")
    val data2 = runInterpretations("tryingGraph", tryingGraph, data1)
    val data3 = runInterpretations("tryingGraph", tryingGraph, data2)

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
