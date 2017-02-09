# utils
Utility classes

[![Build Status](https://travis-ci.org/Gilandel/utils.svg?branch=develop)](https://travis-ci.org/Gilandel/utils)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/e34c82e78aaf45a797721e62a7a31a0a)](https://www.codacy.com/app/gilles/utils)
[![Dependency Status](https://www.versioneye.com/user/projects/58997170c71294003d853a71/badge.svg?style=flat)](https://www.versioneye.com/user/projects/58997170c71294003d853a71)
[![codecov.io](https://codecov.io/github/Gilandel/utils/coverage.svg?branch=develop)](https://codecov.io/github/Gilandel/utils?branch=develop)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/fr.landel/utils/badge.svg)](https://maven-badges.herokuapp.com/maven-central/fr.landel/utils)

[![codecov.io tree](https://codecov.io/gh/Gilandel/utils/branch/develop/graphs/tree.svg)](https://codecov.io/gh/Gilandel/utils/branch/develop)
[![codecov.io sunburst](https://codecov.io/gh/Gilandel/utils/branch/develop/graphs/sunburst.svg)](https://codecov.io/gh/Gilandel/utils/branch/develop)

Beware, the current snapshot has a lot of modifications and is not compatible with the version 0.0.1 available on Maven Central.

# utils-commons

##Commons:
- CastGenerics: To cast map / list / object (avoid the use of @SuppressWarnings
- ClassUtils: To get super classes or to get common super classes
- CollectionUtils: Add missing transform methods (in complement of CollectionUtils provided by Apache Team)
- DateUtils: Extend DateUtils from Apache project, add methods to get date if null, getDate wrapper to secure date transfer and between to calculate time between dates
- EnumChar: A list of ASCII characters and others with their unicode and HTML version
- EnumUtils: Extend EnumUtils from Apache project, add methods to get null if empty name is used (avoid exception)
- HexUtils: To convert hexadecimal in bytes
- NumberUtils: Extend NumberUtils from Apache project, add methods to check number equality
- StringUtils: Extend NumberUtils from Apache project, add methods to get default string if empty or null

##Over:
- AbstractOverComparable: Class to force the implementation of compareTo method
- AbstractOverObject: Class to force implementation of toString, equals and hashCode 

##Listener:
Base classes to manage events (listenable / event / listener)

##Function:
-

##Tuple:
- 

# utils-assertor
A module to validate method parameters.
Based on the version of Assert provided by the Spring Team.

Examples:
```java
Assertor.that(file).isNotNull().and().validates((f) -> f.isDirectory(), "not a directory").toThrow(); // -> if file is not a directory, an IllegalArgumentException is thrown
Assertor.that(paramInt).iGT(10).toThrow(() -> new MyException("invalid")); // -> if conditions are false, a MyException is thrown
Assertor.that(array).isNotEmpty().xor(paramMessage).contains("text").toThrow((errors, parameters) -> new MyException("invalid")); // -> if conditions are false, a MyException is thrown
```

[Link to summary](./utils-assertor#summary)

# utils-io
- CloseableManager: A manager for closeable, to open X closeable and to close them (written before Java7 autocloseable)
- EncodingUtils: All encoding (BOM, String and Charset)
- FileCRC32Utils: To get CRC32 from a file
- FileSizeUtils: List all file sizes from octects to exabioctets
- FileSystemUtils: All methods to manage files and directories
- FileUtils: To read, write and compare files
- StreamUtils: To manage stream files (related to Input/OutputStream)
- SystemUtils: To get operating system info

# utils-log
- MDCMT: To add easily MDC in your application in multi-threaded context

# utils-aop
- AbstractAspect: Base to include AOP (logging and profiling) in an application

# TODO

IO: handle copy/move/delete progress
Model: HQL Builder v2
Mapper: Mapper v3 (more annotation (validator, converter... with supplier/consumer...)