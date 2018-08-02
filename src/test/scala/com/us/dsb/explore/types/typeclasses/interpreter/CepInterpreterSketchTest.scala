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


  sealed trait BaseProcessing {
    def label: String
  }

  case class SequenceProcessing(label: String, steps: BaseProcessing*) extends BaseProcessing

  case class PerEventKindProcessing(label: String, map: (Class[_ <: Event], BaseProcessing)*) extends BaseProcessing

  type LifecycleState = String
  case class PerLifecyleStateProcessing(label: String, map: (LifecycleState, BaseProcessing)*) extends BaseProcessing

  sealed trait PrimitiveProcessing extends BaseProcessing
  case class PrintHiPrimitiveProcessing(label: String) extends PrimitiveProcessing
  case class NamedPrimitiveProcessing(label: String, name: String) extends PrimitiveProcessing



  def evaluate(proc: BaseProcessing, lifeCycleValue: String): Unit = {
    val label = proc.label
    System.err.println(s"(+$label)")
    val value =
      proc match {
        case PrintHiPrimitiveProcessing(xxlabel) =>
          System.err.println("Hi.")
        case NamedPrimitiveProcessing(xxlabel, name) =>
          System.err.println(s"Doing '$name'")
        case SequenceProcessing(xxlabel, steps @ _*) =>
          steps.foreach(evaluate(_, lifeCycleValue))
        case PerEventKindProcessing(xxlabel, map @ _*) =>
          val dummyHardcodedActualEventKind = classOf[StartEvent]
          List(
            {
              for (proc <- map.toMap.get(dummyHardcodedActualEventKind)) yield {
                evaluate(proc, lifeCycleValue)
              }
            }.getOrElse(???)
          )
        case PerLifecyleStateProcessing(xxlabel, map @ _*) =>
          List(
            {
              for (proc <- map.toMap.get(lifeCycleValue)) yield {
                evaluate(proc, lifeCycleValue)
              }
            }.getOrElse(???)
          )
      }
    System.err.println(s"(-$label)")
    value: Unit
  }

  //????? doesn't address non-tree nature of graph
  def format(proc: BaseProcessing): String = {
    def format(indentation: String, proc: BaseProcessing): String = {
      proc match {
        case PrintHiPrimitiveProcessing(label) =>
          indentation + "- '$label': print \"Hi.\""
        case NamedPrimitiveProcessing(label, name) =>
          indentation + s"- '$label': do '$name'"
        case SequenceProcessing(label, steps @ _*) =>
          List(
            indentation + s"- '$label': sequence {",
            steps.map(p => format("  " + indentation, p)).mkString("\n"),
            indentation + s"- } ('$label')"
          ).mkString("\n")
        case PerEventKindProcessing(label, map @ _*) =>
          List(
            indentation + s"- '$label': per event kind: {",
            map.map(pair => {
              indentation + " *" + pair._1.toString +
              "\n" +
              format("  " + indentation, pair._2)
            }).mkString("\n"),
            indentation + s"- } ('$label')"
          ).mkString("\n")
        case PerLifecyleStateProcessing(label, map @ _*) =>
          List(
            indentation + s"- '$label': per lifecycle state: {",
            map.map(pair => {
              indentation + " *" + pair._1.toString +
              "\n" +
              format("  " + indentation, pair._2)
            }).mkString("\n"),
            indentation + s"- } ('$label')"
          ).mkString("\n")
      }
    }
    format("", proc)
  }


  val tryingGraph = {
    val creationEventProcessing =
      NamedPrimitiveProcessing("creationEventProcessing", "CreationEvent processing")

    val startEventProcessing =
      PerLifecyleStateProcessing(
        "startEventProcessing",
        ("draft", NamedPrimitiveProcessing("...1", "StartEvent processing")),
        ("ready", NamedPrimitiveProcessing("...2", "StartEvent: RejectedStartEvent"))
      )

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
      "startEventProcessing",
      preEventProcessing,
      eventProcessing,
      postEventProcessing
    )
  }

  val procGraphs =
    List(
      PrintHiPrimitiveProcessing("someLabel"),
      SequenceProcessing(
        "someLabel",
        PrintHiPrimitiveProcessing("..."),
        PrintHiPrimitiveProcessing("...")),
      SequenceProcessing(
        "someLabel",
        NamedPrimitiveProcessing("...", "Primitive step 1"),
        NamedPrimitiveProcessing("...", "Primitive step 1")),
      tryingGraph
    )


  def runInterpretations(label: String,
                         proc: BaseProcessing
                        ): Unit = {
    System.err.println()
    System.err.println(s"$label                   = " + proc)

    System.err.println(s"format($label):")
    System.err.println(s"----------:")
    System.err.println(s"" + format(proc))
    System.err.println(s":----------")

    List("draft", "ready").foreach(lifeCycleValue => {
      System.err.println(s"evaluate($label, $lifeCycleValue):")
      System.err.println(s"==========:")
      locally {
        val t1: Unit = evaluate(proc, lifeCycleValue)
        System.err.println(s"= = = = = :")
        System.err.println(t1)
      }
      System.err.println(s":==========")
    })


    //??System.err.println(s"simplify($label)         = " + simplify(proc))
    //??System.err.println(s"format(simplify($label)) = " + format(simplify(proc)))
    //??System.err.println(s"listProperties(simplify($label)) = " + listProperties(proc))
    //??assert(evaluate(proc) == evaluate(simplify(proc)))
  }


  procGraphs.zipWithIndex.foreach({case(x, y) =>  runInterpretations("#" + y, x)})


  it(""){

  }
}
