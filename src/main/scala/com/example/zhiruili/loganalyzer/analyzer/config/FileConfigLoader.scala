package com.example.zhiruili.loganalyzer.analyzer.config

import java.io.File

import com.example.zhiruili.loganalyzer.analyzer.config.ConfigLoader.{ConfigLoadingException, NoSuchConfigException}
import com.example.zhiruili.loganalyzer.{Platform, Sdk, Version}

import scala.io.Source
import scala.util.{Failure, Try}

/**
  * 面向文件的配置读取器
  */
trait FileConfigLoader extends ConfigLoader {

  /**
    * 获取配置文件的路径
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @return 配置文件所在文件路径字符串
    */
  def getConfigFilePath(sdk: Sdk, platform: Platform, version: Version): String

  override def loadConfig(sdk: Sdk, platform: Platform, version: Version): Try[AnalyzerConfig] = {
    for {
      configStr <- loadConfigString(sdk, platform, version)
      config <- parser.parseConfigString(configStr)
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
    val configFile = new File(getConfigFilePath(sdk, platform, version))
    if (!configFile.exists) {
      Failure(NoSuchConfigException(sdk, platform, version))
    } else {
      Try { Source.fromFile(configFile).mkString } match {
        case Failure(thw) => Failure(ConfigLoadingException(sdk, platform, version, thw))
        case success => success
      }
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
    * @param configFileName   配置文件名
    * @param configParser     配置文件解析器
    * @return 面向文件的配置读取器
    */
  def createSimpleLoader(rootDirPath: String, configFileName: String, configParser: ConfigParser): FileConfigLoader = {
    new FileConfigLoader {
      override val parser: ConfigParser = configParser
      override def getConfigFilePath(sdk: Sdk, platform: Platform, version: Version): String = {
        s"${new File(rootDirPath).getAbsolutePath}/$sdk/$platform/$version/$configFileName"
      }
    }
  }
}
