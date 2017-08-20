package com.example.zhiruili.loganalyzer.analyzer

import com.example.zhiruili.loganalyzer.analyzer.AnalyzerConfig.Problem
import com.example.zhiruili.loganalyzer.analyzer.ConfigLoader.RecursiveExtendConfigException
import com.example.zhiruili.loganalyzer.{Platform, Sdk, Version}

import scala.util.{Failure, Success, Try}

/**
  * 配置文件加载器
  */
trait ConfigLoader {

  /**
    * 读取配置
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @return   配置，可能出错
    */
  def loadConfig(sdk: Sdk, platform: Platform, version: Version): Try[AnalyzerConfig]

  /**
    * 读取基础配置，递归展开配置继承
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @return   基础配置，可能出错
    */
  def loadRootConfig(sdk: Sdk, platform: Platform, version: Version): Try[RootConfig] =  {

    // 防止递归读取
    def loadHelper(version: Version, verHaveLoaded: Set[Version]): Try[RootConfig] = {
      if (verHaveLoaded(version)) {
        Failure(RecursiveExtendConfigException(sdk, platform, version))
      } else {
        loadConfig(sdk, platform, version) match {
          case Failure(thw) => Failure(thw)
          case Success(cfg@RootConfig(_)) => Success(cfg)
          case Success(ExtendConfig(fromVer, newConfig)) =>
            for {
              baseConfig <- loadHelper(fromVer, verHaveLoaded + version)
            } yield RootConfig(baseConfig.problemBindings ++ newConfig.problemBindings)
        }
      }
    }

    loadHelper(version, Set.empty)
  }

  /**
    * 列出所有问题
    *
    * @param sdk        SDK
    * @return 所有问题的列表，可能出错
    */
  def loadProblemList(sdk: Sdk): Try[List[Problem]]
}

object ConfigLoader {

  /**
    * 配置文件继承出现递归
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    */
  case class RecursiveExtendConfigException(sdk: Sdk, platform: Platform, version: Version)
    extends RuntimeException(s"Recursive extend config of SDK: $sdk, platform: $platform, version: $version")

  /**
    * 配置文件读取异常
    *
    * @param sdk        SDK
    * @param platform   运行平台
    * @param version    版本号
    * @param thw        异常
    */
  case class ConfigLoadingException(sdk: Sdk, platform: Platform, version: Version, thw: Throwable)
    extends RuntimeException(s"Load config error (SDK: $sdk, platform: $platform, version: $version): $thw")

  /**
    * 问题列表读取异常
    *
    * @param sdk        SDK
    * @param thw        异常
    */
  case class ProblemLoadingException(sdk: Sdk, thw: Throwable)
    extends RuntimeException(s"Load problem error (SDK: $sdk): $thw")
}
