package com.example.zhiruili.loganalyzer.rules

import com.example.zhiruili.loganalyzer.logs.{LegalLog, LogItem, LogLevel}

import scala.annotation.tailrec

/**
  * 基础日志
  */
object BasicRules {

  /**
    * 匹配一项日志
    *
    * @param optComment   备注名
    * @param optIsKeyLog  是否为关键路径日志
    * @param optLevel     日志等级
    * @param optPosRegex  日志打印位置（正则字符串）
    * @param optMsgRegex  日志信息（正则字符串）
    * @param extMsgRegex  额外信息（键值对，其中值为正则字符串）
    */
  case class SingleLog(optComment: Option[String],
                       optIsKeyLog: Option[Boolean],
                       optLevel: Option[LogLevel],
                       optPosRegex: Option[String],
                       optMsgRegex: Option[String],
                       extMsgRegex: Map[String, String]) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = {

      def matchHelper(logs: List[LogItem], skippedRev: List[LogItem]): MatchResult = logs match {
        case (log@LegalLog(_, isKey, lv, pos, msg, ext))::remainLogs =>
          def matchIsKey = optIsKeyLog.forall(_ == isKey)
          def matchLevel = optLevel.forall(_ == lv)
          def matchPos = optPosRegex.forall(pos.matches)
          def matchMsg = optMsgRegex.forall(msg.matches)
          def matchExt = extMsgRegex.forall {
            case (key, value) =>
              // TODO extMsgRegex 的值部分的类型改为 Option[String]，并在这里处理匹配
              ext.get(key).exists(_.matches(value))
          }
          if (matchIsKey && matchLevel && matchPos && matchMsg && matchExt) {
            MatchSuccess(List(log), skippedRev.reverse, remainLogs)
          } else {
            MatchFailure(List(log), Nil, Nil, remainLogs)
          }
        case unknownLog::remainLogs => MatchFailure(List(unknownLog), Nil, Nil, remainLogs)
        case Nil => MatchFailure(Nil, Nil, Nil, Nil)
      }

      matchHelper(logs, Nil)
    }
  }

  /**
    * 按顺序匹配一组规则，被匹配的日志必须是紧密排列的
    *
    * @param rules  规则列表
    */
  case class RuleSequence(rules: List[Rule]) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = {

      @tailrec
      def matchHelper(remainLogs: List[LogItem], rules: List[Rule],
                      matchedRev: List[List[LogItem]], skippedRev: List[List[LogItem]]): MatchResult = {
        rules match {
          case Nil =>
            MatchSuccess(matchedRev.reverse.flatten, skippedRev.reverse.flatten, remainLogs)
          case rule::remainRules => rule.matchLogItems(remainLogs) match {
            case MatchFailure(mismatched1, matched1, skipped1, remainLogs1) =>
              MatchFailure(mismatched1, (matched1::matchedRev).reverse.flatten, (skipped1::skippedRev).reverse.flatten, remainLogs1)
            case MatchSuccess(matched1, skipped1, remainLogs1) =>
              matchHelper(remainLogs1, remainRules, matched1::matchedRev, skipped1::skippedRev)
          }
        }
      }

      matchHelper(logs, rules, Nil, Nil)
    }
  }

  /**
    * 匹配一组有序的规则，被匹配的日志不一定连续
    *
    * @param rules  规则列表
    */
  case class RuleOrdered(rules: List[Rule]) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = rules match {
      case Nil => RuleSequence(Nil).matchLogItems(logs)
      case rule::remainRules =>
        RuleSequence(rule::remainRules.map(r => RuleAppear(r))).matchLogItems(logs)
    }
  }

  /**
    * 规则出现指定次数
    *
    * @param rule   规则
    * @param times  出现次数，默认为 1
    */
  case class RuleAppear(rule: Rule, times: Int = 1) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = {

      def matchFromAnyPlace(logs: List[LogItem], skippedRev: List[LogItem]): MatchResult = {
        rule.matchLogItems(logs) match {
          case MatchSuccess(matchLogs, skipLogs, remainLogs) =>
            MatchSuccess(matchLogs, skippedRev.reverse ++ skipLogs, remainLogs)
          case MatchFailure(mismatched1, matched1, skipped1, remain1) => logs match {
            case Nil => MatchFailure(mismatched1, matched1, skippedRev.reverse ++ skipped1, remain1)
            case log::remain => matchFromAnyPlace(remain, log::skippedRev)
          }
        }
      }

      @tailrec
      def matchHelper(times: Int, remain: List[LogItem],
                      matchedRev: List[List[LogItem]], skippedRev: List[List[LogItem]]): MatchResult = {
        if (times <= 0) {
          MatchSuccess(matchedRev.reverse.flatten, skippedRev.reverse.flatten, remain)
        } else {
          matchFromAnyPlace(remain, Nil) match {
            case MatchFailure(mismatched1, matched1, skipped1, remain1) =>
              MatchFailure(mismatched1, (matched1::matchedRev).reverse.flatten, (skipped1::skippedRev).reverse.flatten, remain1)
            case MatchSuccess(matched1, skipped1, remain1) =>
              matchHelper(times - 1, remain1, matched1::matchedRev, skipped1::skippedRev)
          }
        }
      }

      matchHelper(times, logs, Nil, Nil)
    }
  }

  /**
    * 不出现某规则，从任意位置匹配
    *
    * @param rule   规则
    */
  case class RuleNoAppear(rule: Rule) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = {
      RuleNot(RuleAppear(rule)).matchLogItems(logs)
    }
  }

  /**
    * 不匹配某规则，从头匹配
    *
    * @param rule   规则
    */
  case class RuleNot(rule: Rule) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = rule.matchLogItems(logs) match {
      case MatchFailure(mismatched1, matched1, skipped1, remain1) =>
        MatchSuccess(matched1 ++ mismatched1, skipped1, remain1)
      case MatchSuccess(matched1, skipped1, remain1) =>
        MatchFailure(matched1, Nil, skipped1, remain1)
    }
  }

  /**
    * 匹配任意一个规则，仅匹配符合条件的第一个规则（「或」关系）
    *
    * @param rules  规则组
    */
  case class MatchAnyRule(rules: List[Rule]) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = {

      @tailrec
      def matchHelper(rules: List[Rule], mismatchedRev: List[List[LogItem]],
                      matchedRev: List[List[LogItem]], skippedRev: List[List[LogItem]]): MatchResult = {
        rules match {
          case Nil =>
            MatchFailure(mismatchedRev.reverse.flatten, matchedRev.reverse.flatten, skippedRev.reverse.flatten, logs)
          case rule::remainRules => rule.matchLogItems(logs) match {
            case MatchFailure(mismatched1, matched1, skipped1, _) =>
              matchHelper(remainRules, mismatched1::mismatchedRev, matched1::matchedRev, skipped1::skippedRev)
            case matchSuccess => matchSuccess
          }
        }
      }

      matchHelper(rules, Nil, Nil, Nil)
    }
  }

  /**
    * 匹配全部规则（「与」关系），每次都从头开始匹配
    *
    * @param rules  规则组
    */
  case class MatchAllRules(rules: List[Rule]) extends Rule {

    override def matchLogItems(logs: List[LogItem]): MatchResult = {

      @tailrec
      def matchHelper(rules: List[Rule], matchedRev: List[List[LogItem]], skippedRev: List[List[LogItem]]): MatchResult = {
        rules match {
          case Nil =>
            MatchSuccess(matchedRev.reverse.flatten, skippedRev.reverse.flatten, logs)
          case rule::remainRules => rule.matchLogItems(logs) match {
            case MatchFailure(mismatched1, matched1, skipped1, _) =>
              MatchFailure(mismatched1, (matched1::matchedRev).reverse.flatten, (skipped1::skippedRev).reverse.flatten, logs)
            case MatchSuccess(matched1, skipped1, remainLogs1) =>
              matchHelper(remainRules, matched1::matchedRev, skipped1::skippedRev)
          }
        }
      }

      matchHelper(rules, Nil, Nil)
    }
  }
}
