package com.example.zhiruili.loganalyzer.analyzer.config

import java.io.File

import com.example.zhiruili.loganalyzer.analyzer.{AnalyzerConfig, ConfigLoader}
import com.example.zhiruili.loganalyzer.analyzer.ConfigLoader.{ConfigLoadingException, ProblemLoadingException}
import com.example.zhiruili.loganalyzer.{Platform, Sdk, Version}

import scala.io.Source
import scala.util.{Failure, Try}

/**
  * 面向文件的配置读取器
  */
trait FileConfigLoader extends ConfigLoader {

  /**
    * 解析帮助信息
    *
    * @return
    */
  def helpBindingParser: HelpBindingParser

  /**
    * 获取帮助绑定配置文件的路径
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @return 配置文件所在文件路径字符串
    */
  def getHelpBindingFilePath(sdk: Sdk, platform: Platform, version: Version): String

  def problemListParser: ProblemListParser

  /**
    * 获取问题绑定的路径
    *
    * @param sdk      SDK
    * @return 问题绑定文件路径
    */
  def getProblemFilePath(sdk: Sdk): String

  override def loadConfig(sdk: Sdk, platform: Platform, version: Version): Try[AnalyzerConfig] = {
    for {
      configStr <- loadConfigString(sdk, platform, version)
      config <- helpBindingParser.parseConfigString(configStr)
    } yield config
  }

  /**
    * 读取配置文件的内容字符串
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @return 配置文件内容字符串，可能出错
    */
  def loadConfigString(sdk: Sdk, platform: Platform, version: Version): Try[String] = {
    Try { Source.fromFile(getHelpBindingFilePath(sdk, platform, version)).mkString } match {
      case Failure(thw) => Failure(ConfigLoadingException(sdk, platform, version, thw))
      case success => success
    }
  }

  /**
    * 列出所有问题
    *
    * @param sdk SDK
    * @return 所有问题的列表，可能出错
    */
  override def loadProblemList(sdk: Sdk): Try[List[AnalyzerConfig.Problem]] = {
    for {
      configStr <- loadProblemListString(sdk)
      config <- problemListParser.parseConfigString(configStr)
    } yield config
  }

  def loadProblemListString(sdk: Sdk): Try[String] = {
    Try { Source.fromFile(getProblemFilePath(sdk)).mkString } match {
      case Failure(thw) => Failure(ProblemLoadingException(sdk, thw))
      case success => success
    }
  }
}

object FileConfigLoader {

  /**
    * 获取一个简单的 FileConfigLoader 对象
    * 规则文件的查找规则为：
    *   根目录路径(rootDirPath)/SDK 名(sdk)/平台名(platform)/版本号(version)/配置文件名(fileName)
    *
    * @param rootDirPath      配置文件根目录路径
    * @return 面向文件的配置读取器
    */
  def createSimpleLoader(rootDirPath: String, helpConfigName: String, problemConfigName: String): FileConfigLoader = {

    new FileConfigLoader {

      /**
        * 解析帮助信息
        *
        * @return
        */
      override def helpBindingParser: HelpBindingParser = DefaultHelpBindingParser

      /**
        * 获取帮助绑定配置文件的路径
        *
        * @param sdk      SDK
        * @param platform 运行平台
        * @param version  版本号
        * @return 配置文件所在文件路径字符串
        */
      override def getHelpBindingFilePath(sdk: Sdk, platform: Platform, version: Version): String = {
        s"${new File(rootDirPath).getAbsolutePath}/$sdk/$platform/$version/$helpConfigName"
      }

      override def problemListParser: ProblemListParser = DefaultProblemParser

      /**
        * 获取问题绑定的路径
        *
        * @param sdk SDK
        * @return 问题绑定文件路径
        */
      override def getProblemFilePath(sdk: Sdk): String = {
        s"${new File(rootDirPath).getAbsolutePath}/$sdk/$problemConfigName"
      }
    }
  }
}
