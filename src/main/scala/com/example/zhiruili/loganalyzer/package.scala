package com.example.zhiruili

import java.io.File

import scala.io.Codec

package object loganalyzer {

  sealed trait Sdk
  case object ILiveSdk extends Sdk {
    override def toString: String = "ilive"
  }

  sealed trait Platform
  case object PlatformAndroid extends Platform {
    override def toString: String = "android"
  }
  case object PlatformIOS extends Platform {
    override def toString: String = "ios"
  }
  case object PlatformPC extends Platform {
    override def toString: String = "pc"

  }
  case object PlatformOSX extends Platform {
    override def toString: String = "osx"

  }
  case object PlatformWeb extends Platform {
    override def toString: String = "web"
  }

  type Version = String

  val fileSep: String = File.separator

  val encoding: Codec = io.Codec.UTF8

}
