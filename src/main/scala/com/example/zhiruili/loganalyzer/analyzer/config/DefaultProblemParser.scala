package com.example.zhiruili.loganalyzer.analyzer.config

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.{Problem, ProblemTag}
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

object DefaultProblemParser extends ProblemListParser {
  override def parseConfigString(str: String): Try[List[Problem]] = Try {
    Json.parse(str).as[JsObject].fields.toList.map { case (tag, name) =>
      Problem(ProblemTag(tag), name.as[String])
    }
  }
}
