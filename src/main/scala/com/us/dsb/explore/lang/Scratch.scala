package com.us.dsb.explore.lang

class Scratch {

  trait Eqxx[-T, -U]
  implicit def eqString: Eqxx[String, String] = new Eqxx[String, String] {}

  implicitly[Eqxx[String, String]]
}
