package com.us.dsb.explore.jsonapi.poc1

//import io.estatico.newtype.macros.newtype

import java.net.{URI, URLDecoder}
import java.nio.charset.StandardCharsets
import scala.annotation.unused

object RequestInterpretation {

  /**
   * Parses a URL query string into application/x-www-form-urlencoded query
   * parameter mappings, ROUGHLY.  DOES NOT necessarily handle special cases
   * (e.g., delimiter characters in values, error conditions).
   *
   *
   * @param query query portion of a URL, raw (not already percent-decoded)
   * @return sequence of name-to-value mappings, preserving order
   */
  def parseQueryToQueryParameters(queryOpt: Option[String]): Seq[(String, String)]/*QueryParamSeq*/ = {
    def percentDecode(encoded: String): String = {
      //println(s"encoded = '$encoded'")
      URLDecoder.decode(encoded, StandardCharsets.UTF_8.name())
    }

    //println("parseQueryToQueryParameters:  queryOpt = " + queryOpt)
    val mappings = {
      queryOpt.fold(Seq[(String, String)]()) { query =>

        val rawMappings = query.split("&").toSeq
        //println("parseQueryToQueryParameters:  rawMappings = " + rawMappings)

        rawMappings.map { rawMapping =>
          val rawName :: rawValue :: Nil = rawMapping.split("=").toList // .get
          val name = percentDecode(rawName.replace("+", "%20"))
          val value = percentDecode(rawValue.replace("+", "%20"))
          name -> value
        }
      }
    }
    //println("parseQueryToQueryParameters:  mappings = " + mappings)
    mappings
  }

  //???? apply value classes
//  @newtype case class QueryParamSeq(raw: Seq[(String, String)])

  case class InterpretationOne(pathSegments: Seq[String],
                               queryParameters: Seq[(String, String)]/*QueryParamSeq*/  // still ordered
                              )

  def interpretUrlLevel1(url: URI): Either[String, InterpretationOne] = {
    println("interpretUrlLevel1: url = " + url)
    if (false) {
      println("- .getScheme       = " + url.getScheme)
      println("- .getAuthority    = " + url.getAuthority)
      println("- .getRawAuthority = " + url.getRawAuthority)
      println("- .getPath         = " + url.getPath)
      println("- .getRawPath      = " + url.getRawPath)
      println("- .getQuery        = " + url.getQuery)
      println("- .getRawQuery     = " + url.getRawQuery)
      println("- .getFragment     = " + url.getFragment)
      println("- .getRawFragment  = " + url.getRawFragment)
    }

    // - scheme is irrelevant
    // - authority is irrelevant
    // - path should be absolute path
    //   - Q: getPath or getRawPath?  Decode whole path here, or decode per
    //     segment later?  (We don't use logical slashes in path segments.)
    // - query is optional; should be x-www-form-urlencoded query parameters
    // - fragment should be absent

    val decodedPathOpt = Option(url.getPath)

    val decodedPathOrError = decodedPathOpt.toRight[String]("getPath unexpectedly returned null(?)")

    val segmentsOrError =
      decodedPathOrError.flatMap { path =>
        if (! path.startsWith("/") ) {
          Left("path doesn't start with \"/\"")
        }
        else {
          val segments = path.split("/").toList.drop(1)
          println("interpretUrlLevel1: segments = " + segments)
          Right(segments)
        }
      }
    val rawQueryOpt = Option(url.getRawQuery)
    val queryParameterSeq = /*QueryParamSeq*/(parseQueryToQueryParameters(rawQueryOpt))

    println("interpretUrlLevel1: segmentsOrError = " + segmentsOrError)
    println("interpretUrlLevel1: queryParameterSeq = " + queryParameterSeq)

    val interpretation =
      segmentsOrError.map { segments =>
        InterpretationOne(segments, queryParameterSeq)
      }
    println("interpretUrlLevel1: interpretation = " + interpretation)
    interpretation
  }

  def resolveMultipleQueryParameterInstances(rawParametersSeq: Seq[(String, String)]/*QueryParamSeq*/
                                            ): Either[String, Seq[(String, String)]/*QueryParamSeq*/] = {
    val zero: Either[String, Seq[(String, String)]] = Right(Seq())
    val noDupsOrError: Either[String, Seq[(String, String)]/*QueryParamSeq*/] =
      rawParametersSeq/*.raw*/.foldLeft(zero) { case (accum, in) =>
        accum match {
          case error @ Left(_) =>
            error
          case Right(noDupsSoFar) =>
            println("in = " + in + ", accum = " + accum)

            if (noDupsSoFar.exists(_._1 == in._1)) {
              Left(s"repeated query parameter ~'$in', after $noDupsSoFar")
            }
            else {
              Right(noDupsSoFar :+ in)
            }
        }
      }
    println("resolveMultipleQueryParameterInstances: noDupsOrError = " + noDupsOrError)
    noDupsOrError
  }

//???? clean up (enumeration, incomplete parsing)
//??? maybe next step:  collect parameters into class with all known parameters (lists for some with parameters)
//  import enumeratum.Enum
  import enumeratum.EnumEntry

  sealed trait ParameterSpec extends EnumEntry
  object ParameterSpec /*extends Enum[KnownParameter]*/ {

    case class DummyOne(value: String) extends ParameterSpec
    case class DummyTwo(parameter: String, value: String) extends ParameterSpec

//    // - automatic listing of enumerators:
//    val values: IndexedSeq[KnownParameter] = findValues
  }
  import ParameterSpec._

  case class ParametersBlock(dummyOne: Option[DummyOne],
                             dummyTwos: Seq[DummyTwo])




  //??? can I relax Seq to say order doesn't matter? Set isn't right either
  def resolveToKnownParameters(rawParametersSeq: Seq[(String, String)]
                              ): Either[String, Seq[ParameterSpec]] = {
    val zero: Either[String, Seq[ParameterSpec]] = Right(Seq())
    val withNamesResolved =
      rawParametersSeq.foldLeft(zero) { case (accum, (name, value)) =>
        accum match {
          case error@Left(_) =>
            error
          case Right(resolvedSoFar) =>
            name match {
              case "dummyOne" =>
                Right(resolvedSoFar :+ ParameterSpec.DummyOne(value))
              case name if name.startsWith("dummyTwo[") =>
                //???? parse "dummyTwo[param]=value" to DummyTwo("param", "value")
                Right(resolvedSoFar :+ ParameterSpec.DummyTwo("???TBD", value))
              case unrecognized =>
                Left(s"Unrecognized query parameter '$unrecognized' (value = $value) ")
            }
        }
      }
   withNamesResolved
  }


}

object RequestHandlingTemp extends App {
  val x1 = RequestInterpretation.interpretUrlLevel1(new URI("/prefix/users/123?a=1&b=2&a=3"))
  val x2 = x1.toOption.get.queryParameters
  val x3 = RequestInterpretation.resolveMultipleQueryParameterInstances(x2)
  println(s"x1 = " + x1)
  println(s"x2 = " + x2)
  println(s"x3 = " + x3)
//  val x4 = RequestInterpretation.resolveToKnownParameters(x3.toOption.get)
//  println(s"x4 = " + x4)


  val x4a =
    for {
      x1a <- RequestInterpretation.interpretUrlLevel1(
        new URI("/prefix/users/123?dummyOne=1&dummyTwo[x]=y"))
      x2a = x1a.queryParameters
      x3a <- RequestInterpretation.resolveMultipleQueryParameterInstances(x2a)
      x4a <- RequestInterpretation.resolveToKnownParameters(x3a)
    } yield {
      x4a
    }
  println(s"x4a = " + x4a)



  RequestInterpretation.interpretUrlLevel1(new URI(""))
  RequestInterpretation.interpretUrlLevel1(new URI("//host"))

  import RequestInterpretation._

  def analyzeURL(urlStr: String): Unit = {
    println()
    println(s"urlStr = '$urlStr'")
    val url = URI.create(urlStr)


    val path = url.getPath
    println("URL path = " + path)

    assert(path.head == '/')
    val segments = path.split("/").toList.drop(1)
    println("segments = " + segments)

    val query = Option(url.getQuery)  //?? getQuery? getRawQuery?
    println("URL query = " + query)

    val syntacticQueryParamsPairs = parseQueryToQueryParameters(query)
    println("syntacticQueryParamsPairs = " + syntacticQueryParamsPairs)

    val syntacticQueryParamsMap = syntacticQueryParamsPairs.toMap
    println("syntacticQueryParamsMap = " + syntacticQueryParamsMap)

    syntacticQueryParamsMap.foreach { case (name, value@_) =>
      println(s"name = '$name'")
      name match {
        case "a" | "c" | "p" | "p%q" => // known dummy
        case s if s.startsWith("page[") =>
        case "sort" =>
        case s if s.startsWith("fields[") =>
        case "include" =>
        case "filter" =>
        case s if s.startsWith("filter[") =>  //??
        case "customSomething" =>  //??
        case s if s.startsWith("customSomething[") =>  //??
        case _ => ???
      }

      ()
    }

    //??? interpret URL path segments into part of request
    //??? interpret string-level query parameters into part of request

  }


  URI.create("")

  /** (For encoding path and query) */
  def makeURL(path: String, query: String): URI = {
    new URI("scheme", "authority",
            path, query,
            null)
  }

  val url1 = makeURL("/a/b?/c", "a=b&c=w?x#y%26z&p%25q=r&p=q%25r")
  println("url1          = " + url1)
  println("url1.toString = " + url1.toString)
  val urlStr = url1.toString
  analyzeURL(urlStr)



  // NOTE: Hack: no x-www-form-urlencoded syntax or decoding
  val someCases = List(
    "/users/123",
    "/users",

    //?? Which form?  offset + limit?  page size + number?  other?
    "/users?page[size]=10&page[number]=2", // which index origin?
    "/users?page[offset]=10&page[limit]=9",
    // (error if wrong combination)

    "/users?sort=attr1",
    "/users?sort=-attr2,attr3",
    "/users?sort=uToDom.dom_name",
    "/users?sort=unrecommendedNonField",

    "/users?fields[users]=attr1",
    "/users?fields[users]=attr1,rel1",

    "/users?include=uToD,uToGroup.groupToX",

    "/users?filter=x",

    "/users?filter[xxx???]=x",

    "/users?customSomething=x",
    "/users?customSomething[xxx]=x",

    // filter
    // include - comma-separated list of "relationship paths" (each: dot-separated seq. of relationship names)
    // fields
    // sort
    // page?
    )
  someCases.foreach{ url =>
    println()
    println(s"url = '$url'")
    analyzeURL(url)
  }



}