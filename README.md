# 日志分析工具

iLive SDK 日志分析工具，能根据日志内容进行筛选和注释，同时能分析一些简单问题

## 使用方法

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
