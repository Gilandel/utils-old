# CURRENTLY IN DEVELOPMENT

# utils
Utility classes

[![Build Status](https://travis-ci.org/Gilandel/utils.svg?branch=master)](https://travis-ci.org/Gilandel/utils)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/e34c82e78aaf45a797721e62a7a31a0a)](https://www.codacy.com/app/gilles/utils)
[![Dependency Status](https://www.versioneye.com/user/projects/58b29b6f7b9e15003a17e544/badge.svg?style=flat)](https://www.versioneye.com/user/projects/58b29b6f7b9e15003a17e544)
[![codecov.io](https://codecov.io/github/Gilandel/utils/coverage.svg?branch=master)](https://codecov.io/github/Gilandel/utils?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/fr.landel/utils/badge.svg)](https://maven-badges.herokuapp.com/maven-central/fr.landel/utils)

[![codecov.io tree](https://codecov.io/gh/Gilandel/utils/branch/master/graphs/tree.svg)](https://codecov.io/gh/Gilandel/utils/branch/master)
[![codecov.io sunburst](https://codecov.io/gh/Gilandel/utils/branch/master/graphs/sunburst.svg)](https://codecov.io/gh/Gilandel/utils/branch/master)

Beware, the current snapshot has a lot of modifications and is not compatible with the version 0.0.1 available on Maven Central.

# utils-commons
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![Benchmark status](http://vbc3.com/script/progressbar.php?text=Benchmark&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-commons</artifactId>
	<version>1.0.0</version>
</dependency>
```

Purpose:
Base project to group all common features used in many projects.

Complete description: [Link to summary](./utils-commons#summary)

# utils-assertor
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![Benchmark status](http://vbc3.com/script/progressbar.php?text=Benchmark&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=95)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-assertor</artifactId>
	<version>1.0.0</version>
</dependency>
```

A module to validate method parameters.
Original idea is based on the version of Assert provided by the Spring Team.

Examples:
```java
Assertor.that(file).isNotNull().and().validates(f -> f.isDirectory(), "not a directory").toThrow();
// -> if 'file' is null or not a directory, an IllegalArgumentException is thrown

Assertor.that(paramInt).isGT(10).toThrow(() -> new MyException("invalid"));
// -> if 'paramInt' is not greater than 10, a MyException is thrown

Assertor.that(array).isNotEmpty().xor(paramMessage).contains("text").toThrow((errors, parameters) -> new MyException("invalid"));
// -> if 'array' is null/empty and 'paramMessage' does not contain 'text', a MyException is thrown
// -> if 'array' is not null/empty and 'paramMessage' contains 'text', a MyException is thrown
// -> if 'array' is null/empty and 'paramMessage' contains 'text', returns 'paramMessage'
// -> if 'array' is not null/empty and 'paramMessage' does not contain 'text', returns 'paramMessage'

Assertor.that(param1).isNotNull().and().not().startsWith("_")
	.and(param2).isNotNull().and().isBefore(Calendar.getInstance())
	.toThrow("Param1 '%1$s*' or/and param2 '%3$tF* %3$tT*.%3$tL* %3$tZ*' are incorrect");
// -> if condition result is false, throw an IllegalArgumentException with the message "Param1 '<value_param1>' or/and param2 '<value_param2>' are incorrect".
// -> the message uses String.format, to get injected parameter just add an asterisk at the end.

// Exactly the same validation but with more specific errors
Assertor.that(param1).isNotNull("Param1 cannot be null or empty").and().not().startsWith("_", "Param1 '%1$*' cannot start with '%2$s*'")
	.and(param2).isNotNull("Param2 cannot be null").and().isBefore(Calendar.getInstance(), "Param2 '%1$tF* %1$tT*.%1$tL* %1$tZ*' must be before '%2$tF* %2$tT*.%2$tL* %2$tZ*'")
	.toThrow();
// -> throw for example (if param1="test", param2=Calendar[2018-02-28 04:31:56.399 CET] and NOW=Calendar[2017-02-28 04:31:56.401 CET]):
// IllegalArgumnentException("Param2 '2018-02-28 04:31:56.399 CET' must be before '2017-02-28 04:31:56.401 CET'")
```

Complete description: [Link to summary](./utils-assertor#summary)

# utils-io
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-io</artifactId>
	<version>1.0.0</version>
</dependency>
```

Features:
- CloseableManager: A manager for closeable, to open X closeable and to close them (written before Java7 autocloseable)
- EncodingUtils: All encoding (BOM, String and Charset)
- FileCRC32Utils: To get CRC32 from a file
- FileSizeUtils: List all file sizes from octects to exabioctets
- FileSystemUtils: All methods to manage files and directories
- FileUtils: To read, write and compare files
- StreamUtils: To manage stream files (related to Input/OutputStream)
- SystemUtils: To get operating system info

# utils-log
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-log</artifactId>
	<version>1.0.0</version>
</dependency>
```

Features:
- MDCMT: To add easily MDC in your application in multi-threaded context

# utils-aop
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-aop</artifactId>
	<version>1.0.0</version>
</dependency>
```

Features:
- AbstractAspect: Base to include AOP (logging and profiling) in an application with an easy way to get methods signature,
- AOPException: Exception generated by the AbstractAscpect class.

# utils-mapper
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=10)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=5)
![Benchmark status](http://vbc3.com/script/progressbar.php?text=Benchmark&progress=0)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=0)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-mapper</artifactId>
	<version>1.0.0</version>
</dependency>
```

# utils-model
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=60)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=30)
![Benchmark status](http://vbc3.com/script/progressbar.php?text=Benchmark&progress=0)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=10)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-model</artifactId>
	<version>1.0.0</version>
</dependency>
```

# utils-poi
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-poi</artifactId>
	<version>1.0.0</version>
</dependency>
```

Features:
- AssertXLS: Utility class to compare two XLS files with POI.

# utils-template
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![Benchmark status](http://vbc3.com/script/progressbar.php?text=Benchmark&progress=0)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=90)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-template</artifactId>
	<version>1.0.0</version>
</dependency>
```

Purpose:
A simple template tool (very light (around 18kb) and powerful).

Features:
- AbstractScriptsTemplate: Base class to define all properties of a template,
- ScriptsList: Interface that the enumeration containing the list of all scripts has to implement,
- ScriptsLoader: The class that loads all the templates in memory,
- ScriptsReplacer: The class that replaces all the values and generates the formatted output,
- ScriptsTemplate: A class that contains a list of predefined templates: SQL, JSON (ElasticSearch or MongoDB).

Complete description: [Link to summary](./utils-template#summary)

# utils-web
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-web</artifactId>
	<version>1.0.0</version>
</dependency>
```

Features:
- HTCLoader: A simple loader for [PIE](http://css3pie.com/),
- ResourceBundleUTF8Control: A Control class to load all resources file in UTF-8 (mainly to avoid multiple encodings in a Web application).

# utils-microbenchmark
Work progress:
![Code status](http://vbc3.com/script/progressbar.php?text=Code&progress=100)
![Test status](http://vbc3.com/script/progressbar.php?text=Test&progress=100)
![JavaDoc status](http://vbc3.com/script/progressbar.php?text=JavaDoc&progress=100)

```xml
<dependency>
	<groupId>fr.landel.utils</groupId>
	<artifactId>utils-microbenchmark</artifactId>
	<version>1.0.0</version>
</dependency>
```

Features:
- AbstractMicrobenchmark: An abstract class to easily create JMH test cases

# Roadmap

IO: handle copy/move/delete progress