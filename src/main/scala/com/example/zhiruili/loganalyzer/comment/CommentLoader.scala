package com.example.zhiruili.loganalyzer.comment

import com.example.zhiruili.loganalyzer
import com.example.zhiruili.loganalyzer.{Platform, Sdk}
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.io.Source
import scala.util.Try

trait CommentLoader {
  def loadCommentBindings(sdk: Sdk, platform: Platform): Try[CommentBindings]
}

/**
  *
  * $baseDir/$errorFileName
  * {
  *   "xxxModule": {
  *     "1": "xxx error comment 1",
  *     "2": "xxx error comment 2"
  *   },
  *   "yyyModule": {
  *     "101": "yyy error comment 1",
  *     "102": "yyy error comment 2",
  *   }
  *
  * }
  *
  * $baseDir/$sdk/$platform/$generalFileName
  * {
  *   "distinct": {
  *     "message abc": "comment abc",
  *     "message def": "comment def"
  *   },
  *   "fuzzy": {
  *     "message abc": "comment abc",
  *     "message def": "comment def"
  *   }
  * }
  */
object CommentLoader {

  type RawDistinctBindings = List[(String, String)]
  type RawFuzzyBindings = List[(String, String)]

  def ofFile(baseDir: String, errorFileName: String, generalFileName: String): CommentLoader = {

    new CommentLoader {

      def loadCommentBindings(sdk: Sdk, platform: Platform): Try[CommentBindings] = {
        import loganalyzer.fileSep
        val errPath = s"""$baseDir$fileSep$errorFileName"""
        val genPath = s"""$baseDir$fileSep$sdk$fileSep$platform$fileSep$generalFileName"""
        println(s"errPath = $errPath")
        println(s"genPath = $genPath")
        for {
          errContent <- Try { Source.fromFile(errPath)(loganalyzer.encoding).mkString }
          genContent <- Try { Source.fromFile(genPath)(loganalyzer.encoding).mkString }
          errObj <- Try { Json.parse(errContent).as[JsObject] }
          genObj <- Try { Json.parse(genContent).as[JsObject] }
          errRes <- parseErrorBindings(errObj)
          (distinct, fuzzy) <- parseGeneralBindings(genObj)
        } yield CommentBindings(errRes, distinct, fuzzy)
      }

      def parseErrorBindings(bindingObj: JsObject): Try[List[(String, List[(Int, String)])]] = {

        def parseErrorModule(errCodeBinds: JsValue): Try[Stream[(Int, String)]] = Try {
          errCodeBinds.as[JsObject].fields.toStream.map {
            case (codeStr, commentJsVal) => (codeStr.toInt, commentJsVal.as[String])
          }
        }

        bindingObj
          .fields
          .toStream
          .map { case (moduleName, errCodeBinds) =>
            (moduleName, parseErrorModule(errCodeBinds))
          }
          .foldLeft(Try(List.empty[(String, List[(Int, String)])])) { case (tryList, (moduleName, tryCodeBinds)) =>
              for {
                currList <- tryList
                codeBinds <- tryCodeBinds
              } yield (moduleName, codeBinds.toList)::currList
          }
          .map(_.reverse)
      }

      def parseGeneralBindings(rawBindings: JsObject): Try[(RawDistinctBindings, RawFuzzyBindings)] = Try {
        val distinctBindings =
          (rawBindings \ "distinct")
            .as[JsObject]
            .fields
            .toList
            .map { case (msg, commentJsVal) => (msg, commentJsVal.as[String]) }
        val fuzzyBindings = (rawBindings \ "fuzzy").as[JsObject]
          .as[JsObject]
          .fields
          .toList
          .map { case (msg, commentJsVal) => (msg, commentJsVal.as[String]) }
        (distinctBindings, fuzzyBindings)
      }
    }
  }
}
