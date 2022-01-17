package com.us.dsb.explore.lang

object Scratch extends App {

  locally {
    trait Eqxx[-T, -U]

    implicit def eqString: Eqxx[String, String] = new Eqxx[String, String] {}

    implicitly[Eqxx[String, String]]
  }


}
