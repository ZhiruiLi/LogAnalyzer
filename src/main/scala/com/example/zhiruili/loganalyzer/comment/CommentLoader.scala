package com.example.zhiruili.loganalyzer.comment

import com.example.zhiruili.loganalyzer.Sdk
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.io.Source
import scala.util.Try

trait CommentLoader {
  def loadCommentBindings(sdk: Sdk): Try[CommentBindings]
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
  * $baseDir/$sdk/$generalFileName
  * {
  *   "message abc": "comment abc",
  *   "message def": "comment def"
  * }
  */
object CommentLoader {

  def ofFile(baseDir: String, errorFileName: String, generalFileName: String): CommentLoader = {

    new CommentLoader {

      def loadCommentBindings(sdk: Sdk): Try[CommentBindings] = {
        val errPath = s"""$baseDir/$errorFileName"""
        val genPath = s"""$baseDir/$sdk/$generalFileName"""
        for {
          errContent <- Try { Source.fromFile(errPath).mkString }
          genContent <- Try { Source.fromFile(genPath).mkString }
          errObj <- Try { Json.parse(errContent).as[JsObject] }
          genObj <- Try { Json.parse(genContent).as[JsObject] }
          errRes <- parseErrorBindings(errObj)
          genRes <- parseGeneralBindings(genObj)
        } yield CommentBindings(errRes, genRes)
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

      def parseGeneralBindings(rawBindings: JsObject): Try[List[(String, String)]] = Try {
        rawBindings.fields.toList.map { case (msg, commentJsVal) => (msg, commentJsVal.as[String]) }
      }
    }
  }
}
