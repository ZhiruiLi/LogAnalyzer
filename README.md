# 日志分析工具

[iLive SDK](https://www.qcloud.com/product/ilvb) 日志分析工具，能根据日志内容进行筛选和注释，同时能分析一些简单问题

## 工程配置方法

工程使用 [sbt](http://www.scala-sbt.org/) 进行构建，需要先安装该工具。clone 工程到本地，调用 `sbt run` 即可。

```bash
git clone https://github.com/ZhiruiLi/LogAnalyzer
cd LogAnalyzerApp
sbt run
```

执行单元测试:

```bash
sbt test
```

工程同时支持 [sbt-assembly](https://github.com/sbt/sbt-assembly) 进行打包:

```bash
sbt assembly
```

这将在工程目录下生成 `run.jar` 文件，双击即可运行。

**Tips**: 在内网中使用的时候，sbt 需要设置代理服务器才能正确获取依赖。

Windows:

```bat
set JAVA_OPTS=-Dhttps.proxyHost=dev-proxy.oa.com -Dhttps.proxyPort=8080
sbt run
```

OSX:

```
export JAVA_OPTS="$JAVA_OPTS -Dhttps.proxyHost=dev-proxy.oa.com -Dhttps.proxyPort=8080"
sbt run
```

## 使用说明

- 本程序是一个针对 iLive SDK 日志的日志浏览分析器，目前日志分析器支持新旧版日志的浏览和新版日志的浏览与分析。

- 运行需要安装 Java 8 运行环境：<https://www.java.com/zh_CN/>。

- 双击run.jar 文件运行程序，开启后选择平台并导入日志文件，同时也支持直接把日志文件拖放到程序主界面上来打开文件：

  ![load_log_file](https://raw.githubusercontent.com/ZhiruiLi/LogAnalyzer/master/imgs/load_log_file.png)

- 基本界面说明

  日志导入后界面如下：

  ![main_screen](https://raw.githubusercontent.com/ZhiruiLi/LogAnalyzer/master/imgs/main_screen.png)

  在选定发生的问题之后，点击开始分析按钮，进行日志分析。在开始分析之前可以划定时间范围，以便进行更精确的分析。执行之后会给出建议。

  **注意：旧版日志不支持日志分析功能，仅能浏览日志，对旧版日志进行分析的结果没有参考意义。**

  ![help_info](https://raw.githubusercontent.com/ZhiruiLi/LogAnalyzer/master/imgs/help_info.png)

- 原始日志浏览界面说明

  - 格式

    在日志浏览界面中，每一项日志会被格式化为：

    `[日志打印时间] 日志信息 (额外信息) [日志打印位置][日志等级]`

    其中，额外信息以键值对的形式展示，内容为多个「键 -> 值」的形式。

    日志等级分别被标识为：

    Verbose：`V`， Debug：`D`，Info：`I`，Warn：`W`，Error: `E`

    如果日志是关键路径日志，在前面会有一个 `*` 符号标记。

  - 日志颜色

    对于不同等级的日志，会有颜色区分：

    Verbose：灰色；Debug：黑色；Info：黑色；Warn：黄色；Error：红色；

    对于无法正确解析的日志，会标注为灰色。

  - 日志过滤

    日志浏览界面上方有一些能够对日志进行过滤的选项，例如是否关键日志、最低日志等级、日志信息等。

    对于日志信息和打印位置而言，在没有勾选旁边的「严格匹配」选框时，使用模糊匹配，例如 `a b` 可以匹配到 `AB`、`ACB`、`aCb` 等。勾选严格匹配之后可以自己输入正则表达式进行筛选。

    日志筛选的时间的格式是 `yy-MM-dd HH:mm:ss` 也就是 2017 年 1 月 3 日 下午 3 点 10 分 2 秒应该写为：`17-01-03 15:10:02` 。

    ![log_filter](https://raw.githubusercontent.com/ZhiruiLi/LogAnalyzer/master/imgs/log_filter.png)

- Q & A

  - 为什么需要安装 Java 环境？

  - 运行程序直接崩溃是怎么回事？

    有可能是 Java 环境没有配置对，Mac 用户可以试试在命令行下使用 `java -jar run.jar`，Windows 用户可以试试在命令行下使用 `java -jar .\run.jar` 看是否能运行。另外要注意将 config 文件夹和 run.jar 放在同一路径下，程序的执行需要依赖 config 里的配置文件。

  - 如果有问题或者改进意见如何发表？

    可以在这个页面上面提 issue 或者修改后提 pull request。

