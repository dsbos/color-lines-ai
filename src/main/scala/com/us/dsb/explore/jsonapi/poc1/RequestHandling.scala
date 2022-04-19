package com.us.dsb.explore.jsonapi.poc1

import java.net.{URI, URLDecoder}
import java.nio.charset.StandardCharsets

object RequestHandling {

  /**
   * Parses a URL query string into application/x-www-form-urlencoded query
   * parameter mappings, ROUGHLY.  DOES NOT necessarily handle special cases
   * (e.g., delimiter characters in values, error conditions).
   *
   *
   * @param query query portion of a URL; whether raw or percent-decoded is
   *              UNSPECIFIED
   * @return sequence of name-to-value mappings, preserving order
   */
  def parseQueryToQueryParameters(queryOpt: Option[String]): Seq[(String, String)] = {
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

}

object RequestHandlingTemp extends App {
  import RequestHandling._

  def nameThis(urlStr: String): Unit = {
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

    syntacticQueryParamsMap.foreach { case (name, value) =>
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
  nameThis(urlStr)



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
  someCases.foreach{ nameThisx =>
    println()
    println(s"nameThisx = '$nameThisx'")
    nameThis(nameThisx)
  }



}