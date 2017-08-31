package com.example.zhiruili.loganalyzer.comment

import scala.util.matching.Regex

/**
  * 注释信息绑定
  */
trait CommentBindings {

  // 错误信息绑定，键为错误模块名，值为另一个 Map，其中键为错误码，值为错误信息
  def errorBindings: Map[String, Map[Int, String]]

  // 严格区分的信息绑定，当信息与键完全相同时才认为匹配
  def distinctBindings: Map[String, String]

  // 模糊信息绑定，当键匹配到一部分键时就可以匹配
  def fuzzyBindings: Iterable[(Regex, String)]

  /**
    * 根据错误码和报错模块精确匹配错误信息
    *
    * @param code     错误码
    * @param module   报错模块名
    * @return 注释信息，可能为空
    */
  def matchError(code: Int, module: String): Option[String] = errorBindings.get(module).flatMap(_.get(code))

  /**
    * 仅通过错误码来匹配错误信息，由于不同模块可能出现同样的错误码，所以此处的结果为一个 List
    *
    * @param code   错误码
    * @return 注释信息列表，可能为空列表
    */
  def matchErrorCode(code: Int): List[String] = errorBindings.flatMap { case (_, map) => map.get(code) }.toList

  /**
    * 模糊匹配日志信息进行注释
    *
    * @param message  日志信息
    * @return 注释信息列表
    */
  def matchFuzzyMessage(message: String): List[String] = {
    fuzzyBindings
      .filter { case (regex, _) => regex.findFirstIn(message).isDefined }
      .map(_._2)
      .toList
  }

  /**
    * 精确匹配日志信息进行注释
    *
    * @param message  日志信息
    * @return 注释信息
    */
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
