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


  //???? name: ...Computation?  ...Calculation?  ...Comp?  ...Calc?
  //           ...Action? ...Step?  ...Opertation?  *** ...Op? ***
  sealed trait BaseProcessing {
    def label: String
  }

  case class SequenceProcessing(label: String, steps: BaseProcessing*)
      extends BaseProcessing

  case class PerEventKindProcessing(label: String, map: (Class[_ <: Event], BaseProcessing)*)
      extends BaseProcessing

  type LifecycleState = String
  case class PerLifecyleStateProcessing(label: String, map: (LifecycleState, BaseProcessing)*)
      extends BaseProcessing

  case class SetLifecycleProcessing(label: String, lifecycleState: LifecycleState)
      extends BaseProcessing


  sealed trait ProcessingKind
  case class DummyClearMiscData()                extends ProcessingKind
  case class DummySuffixMiscData(suffix: String) extends ProcessingKind

  case class KnownPrimitiveProcessing(label: String, kind: ProcessingKind)
      extends BaseProcessing

  case class NamedPrimitiveProcessing(label: String, name: String)
      extends BaseProcessing

  case class CustomPrimitiveProcessing(label: String, f: InAndOutData => InAndOutData)
      extends BaseProcessing


  case class InAndOutData(eventKind: Class[_ <: Event],
                          lifecycleState: LifecycleState,
                          miscData: String)

  def evaluate(proc: BaseProcessing, data: InAndOutData): InAndOutData = {

    def evaluateKP(proc: KnownPrimitiveProcessing, data: InAndOutData): InAndOutData = {
      val label = proc.label
      proc.kind match {
        case DummySuffixMiscData(suffix) =>
          data.copy(miscData = data.miscData + suffix)
      }
    }

    val label = proc.label
    System.err.println(s"(+$label)")
    val value: InAndOutData =
      proc match {
        case proc: KnownPrimitiveProcessing =>
          evaluateKP(proc, data)
        case NamedPrimitiveProcessing(_, name) =>
          System.err.println(s"Doing '$name'")
          data
        case CustomPrimitiveProcessing(_, fn) =>
          System.err.println(s"Calling <custom function>")
          fn(data)
        case SetLifecycleProcessing(_, lifecycleState) =>
          data.copy(lifecycleState = lifecycleState)
        case SequenceProcessing(_, steps @ _*) =>
          var dataN = data
          steps.foreach(proc => dataN = evaluate(proc, dataN))
          dataN
        case PerEventKindProcessing(_, map @ _*) =>
          locally {
            for (proc <- map.toMap.get(data.eventKind)) yield {
              evaluate(proc, data)
            }
          }.getOrElse(???)
        case PerLifecyleStateProcessing(_, map @ _*) =>
          locally {
            for (proc <- map.toMap.get(data.lifecycleState)) yield {
              evaluate(proc, data)
            }
          }.getOrElse(???)
      }
    System.err.println(s"(-$label): value = " + value)
    value
  }

  //????? doesn't address non-tree nature of graph (reconvergence)
  def format(proc: BaseProcessing): String = {


    def formatXxx(indentation: String, proc: KnownPrimitiveProcessing): String = {
      val label = proc.label
      proc.kind match {
        case DummySuffixMiscData(suffix) =>
          indentation + s"- '$label': <custom function>"
      }
    }

    def formatSub(indentation: String, proc: BaseProcessing): String = {
      proc match {
        case proc: KnownPrimitiveProcessing =>
          formatXxx(indentation, proc)
        case NamedPrimitiveProcessing(label, name) =>
          indentation + s"- '$label': do '$name'"
        case CustomPrimitiveProcessing(label, fn) =>
          indentation + s"- '$label': <custom function>"
        case SetLifecycleProcessing(label, value) =>
          indentation + s"- '$label': set lifecycleState to '$value'"
        case SequenceProcessing(label, steps @ _*) =>
          List(
            indentation + s"- '$label': sequence {",
            steps.map(p => formatSub("  " + indentation, p)).mkString("\n"),
            indentation + s"- } ('$label')"
          ).mkString("\n")
        case PerEventKindProcessing(label, map @ _*) =>
          List(
            indentation + s"- '$label': per event kind: {",
            map.map(pair => {
              indentation + " *" + pair._1.toString +
              "\n" +
              formatSub("  " + indentation, pair._2)
            }).mkString("\n"),
            indentation + s"- } ('$label')"
          ).mkString("\n")
        case PerLifecyleStateProcessing(label, map @ _*) =>
          List(
            indentation + s"- '$label': per lifecycle state: {",
            map.map(pair => {
              indentation + " *" + pair._1.toString +
              "\n" +
              formatSub("  " + indentation, pair._2)
            }).mkString("\n"),
            indentation + s"- } ('$label')"
          ).mkString("\n")
      }
    }
    formatSub("", proc)
  }


  val tryingGraph = {
    val creationEventProcessing =
      SequenceProcessing(
        "creationEventProcessing",
        NamedPrimitiveProcessing("creationEventProcessing", "CreationEvent processing"),
        SetLifecycleProcessing("...", "draft")
      )


    val startEventProcessing = {
      val badStartProcessing =
        NamedPrimitiveProcessing("badStartProcessing", "StartEvent: RejectedStartEvent")
      PerLifecyleStateProcessing(
        "startEventProcessing",
        ("draft",
            SequenceProcessing(
              "...",
              NamedPrimitiveProcessing("(good start.1)", "StartEvent processing"),
              SetLifecycleProcessing("(good start.2)", "ready"),
              CustomPrimitiveProcessing(
                "(good start.3)",
                data => data.copy(miscData = data.miscData + "_suffix1")),
              KnownPrimitiveProcessing("(good start.3)",
                                       DummySuffixMiscData("_suffix2")))
        ),
        ("ready", badStartProcessing),
        ("inTransit", badStartProcessing)
      )
    }

    val preEventProcessing = NamedPrimitiveProcessing("preEventProcessing", "pre-event processing")

    val eventProcessing  =
      PerEventKindProcessing(
        "eventProcessing",
        (classOf[CreationEvent], creationEventProcessing),
        (classOf[StartEvent], startEventProcessing)
      )

    val postEventProcessing =
      NamedPrimitiveProcessing("postEventProcessing", "post-event processing")

    SequenceProcessing(
      "messageProcessing",
      preEventProcessing,
      eventProcessing,
      postEventProcessing
    )
  }

  val basicGraphs =
    List(
      SequenceProcessing(
        "someLabel",
        NamedPrimitiveProcessing("...", "Primitive step 1"),
        NamedPrimitiveProcessing("...", "Primitive step 1"))
    )


  def runInterpretations(label: String,
                         proc: BaseProcessing,
                         data: InAndOutData
                        ): InAndOutData = {
    System.err.println()
    System.err.println(s"$label                   = " + proc)

    System.err.println(s"format($label):")
    System.err.println(s"----------:")
    System.err.println(s"" + format(proc))
    System.err.println(s":----------")

    System.err.println(s"evaluate($label, $data):")
    System.err.println(s"==========:")
    val result = evaluate(proc, data)
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
