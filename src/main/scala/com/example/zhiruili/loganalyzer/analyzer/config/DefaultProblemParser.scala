package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{Problem, ProblemTag}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

/**
  * 默认的问题绑定解析器
  * 格式为：
  * {
  *   "problem tag 1": "problem name 1",
  *   "problem tag 2": "problem name 2"
  * }
  * 其中， problem tag 就是帮助绑定里的 problem tag，而 problem name 是用于展示给用户看的信息
  */
object DefaultProblemParser extends ProblemListParser {
  override def parseConfigString(str: String): Try[List[Problem]] = Try {
    Json.parse(str).as[JsObject].fields.toList.map { case (tag, name) =>
      Problem(ProblemTag(tag), name.as[String])
    }
  }
}
