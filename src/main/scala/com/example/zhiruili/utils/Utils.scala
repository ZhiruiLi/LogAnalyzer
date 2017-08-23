package com.example.zhiruili.utils

import scala.util.matching.Regex

object Utils {

  /**
    * 当两个 Option 参数都不为 None 的时候进行比较
    *
    * @param optA       第一个参数
    * @param optB       第二个参数
    * @param testFunc   测试函数，仅在 optA 和 optB 都不为 None 的时候被调用
    * @tparam A         第一个参数的内部类型
    * @tparam B         第二个参数的内部类型
    * @return 比较结果，如果两个参数有一个或以上是 None，则比较结果为 true
    */
  def testIfDefined[A, B](optA: Option[A], optB: Option[B], testFunc: (A, B) => Boolean): Boolean = {
    (for {
      a <- optA
      b <- optB
    } yield testFunc(a, b)).getOrElse(true)
  }

  // 正则表达式中的特殊字符
  val regexChars: Set[Char] = "\\^$.[]*+?{}|-,>".toSet

  /**
    * 创建一个用于模糊匹配的正则表达式
    *
    * @param str  匹配字符串
    * @return 正则字符串
    */
  def createFuzzyRegex(str: String): String = {
    str
      .flatMap { c =>
        if (c.isLetter)
          s"[${c.toUpper}${c.toLower}]"
        else if (c.isSpaceChar)
          s".*"
        else if (regexChars(c))
          s"\\$c"
        else s"$c"
      }
  }


}
