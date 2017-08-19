package com.example.zhiruili.loganalyzer.rules

import java.io.File

import com.example.zhiruili.loganalyzer.rules.RuleLoader.{NoSuchRuleException, RuleLoadingException}
import com.example.zhiruili.loganalyzer.{Platform, Sdk, Version}

import scala.io.Source
import scala.util.{Failure, Try}

/**
  * 面向本地文件的规则读取器
  */
trait FileRuleLoader extends RuleLoader {

  /**
    * 获取规则文件的路径
    *
    * @param sdk        SDK 名
    * @param platform   运行平台
    * @param version    版本号
    * @param ruleName   规则名
    * @return 规则文件所在文件路径字符串
    */
  def getRuleFilePath(sdk: Sdk, platform: Platform, version: Version)(ruleName: String): String

  def loadIncompleteRule(sdk: Sdk, platform: Platform, version: Version)(ruleName: String): Try[IncompleteRule] =
    for {
      ruleString <- loadRuleString(sdk, platform, version)(ruleName)
      res <- parser.parseRuleString(ruleString)
    } yield res

  def loadRuleString(sdk: Sdk, platform: Platform, version: Version)(ruleName: String): Try[String] = {
    val ruleFile = new File(getRuleFilePath(sdk, platform, version)(ruleName))
    if (!ruleFile.exists) {
      Failure(NoSuchRuleException(sdk, platform, version, ruleName))
    } else {
      Try { Source.fromFile(ruleFile).mkString } match {
        case Failure(thw) => Failure(RuleLoadingException(sdk, platform, version, ruleName, thw))
        case success => success
      }
    }
  }
}

object FileRuleLoader {

  /**
    * 获取一个简单的 FileRuleLoader 对象
    * 规则文件的查找规则为：
    *   根目录路径(rootDirPath)/SDK 名(sdk)/平台名(platform)/版本号(version)/规则名(ruleName).json
    *
    * @param rootDirPath  规则文件根目录路径
    * @param ruleParser   规则文件解析器
    * @return 面向文件的规则读取器
    */
  def createSimpleLoader(rootDirPath: String, ruleParser: RuleParser): FileRuleLoader = {
    new FileRuleLoader {
      override val parser: RuleParser = ruleParser
      override def getRuleFilePath(sdk: Sdk, platform: Platform, version: Version)(ruleName: String): String = {
        s"${new File(rootDirPath).getAbsolutePath}/$sdk/$platform/$version/$ruleName.json"
      }
    }
  }
}

