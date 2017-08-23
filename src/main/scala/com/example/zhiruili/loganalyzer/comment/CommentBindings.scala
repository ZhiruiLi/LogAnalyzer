package com.example.zhiruili.loganalyzer.comment

import scala.util.matching.Regex

trait CommentBindings {

  def errorBindings: Map[String, Map[Int, String]]
  def distinctBindings: Map[String, String]
  def fuzzyBindings: Iterable[(Regex, String)]

//    errorBindings

//  distinctBindings.toMap

  def matchError(code: Int, module: String): Option[String] = errorBindings.get(module).flatMap(_.get(code))

  def matchErrorCode(code: Int): List[String] = errorBindings.flatMap { case (_, map) => map.get(code) }.toList

  def matchFuzzyMessage(message: String): List[String] = {
    fuzzyBindings
      .filter { case (regex, _) => regex.findFirstIn(message).isDefined }
      .map(_._2)
      .toList
  }

  def matchDistinctMessage(message: String): Option[String] = {
    distinctBindings.get(message)
  }
}

object CommentBindings {

  def apply(errorsMap: Iterable[(String, Iterable[(Int, String)])],
            distinctMsgMap: Iterable[(String, String)],
            fuzzyMsgMap: Iterable[(String, String)]): CommentBindings = {

    new CommentBindings {
      val errorBindings: Map[String, Map[Int, String]] =
        errorsMap.map { case (name, mapping) => (name, mapping.toMap) }.toMap
      val distinctBindings: Map[String, String] = distinctMsgMap.toMap
      val fuzzyBindings: Iterable[(Regex, String)] =
        fuzzyMsgMap.map { case (regexString, comment) => (regexString.r, comment) }
    }
  }
}
