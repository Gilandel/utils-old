# TODO
fichiers AUB (queries)  et LOKI (xls) a supprimer
ecrire d'autres TU
ecrire mapper + include converter or supplier/consumer

# utils
Utility classes

[![Build Status](https://travis-ci.org/Gilandel/utils.svg?branch=develop)](https://travis-ci.org/Gilandel/utils)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/e34c82e78aaf45a797721e62a7a31a0a)](https://www.codacy.com/app/gilles/utils)
[![Dependency Status](https://www.versioneye.com/user/projects/571407adfcd19a00415b1a84/badge.svg?style=flat)](https://www.versioneye.com/user/projects/571407adfcd19a00415b1a84)
[![codecov.io](https://codecov.io/github/Gilandel/utils/coverage.svg?branch=develop)](https://codecov.io/gh/Gilandel/utils/branch/develop)

[![codecov.io tree](https://codecov.io/gh/Gilandel/utils/branch/develop/graphs/tree.svg)](https://codecov.io/gh/Gilandel/utils/branch/develop)
[![codecov.io sunburst](https://codecov.io/gh/Gilandel/utils/branch/develop/graphs/sunburst.svg)](https://codecov.io/gh/Gilandel/utils/branch/develop)

# utils-commons

##Commons:
- Cast generics: To cast map / list / object (avoid the use of @SuppressWarnings
- Class utils: To get super classes or to get common super classes
- Collection utils: Add missing transform methods (in complement of CollectionUtils provided by Apache Team)
- Date utils: Extend DateUtils from Apache project, add methods to get date if null and getDate wrapper to secure date transfer
- Enum char: A list of ASCII characters and others with their unicode and HTML version
- Enum utils: Extend EnumUtils from Apache project, add methods to get null if empty name is used (avoid exception)
- Hex utils: To convert hexadecimal in bytes
- Number utils: Extend NumberUtils from Apache project, add methods to check number equality
- String utils: Extend NumberUtils from Apache project, add methods to get default string if empty or null

##Over:
- AbstractOverComparable: Class to force the implementation of compareTo method
- AbstractOverObject: Class to force implementation of toString, equals and hashCode 

##Listener:
Base classes to manage events (listenable / event / listener)

##Function:
-

##Tuple:
- 

# utils-assetor
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
- Closeable manager: A manager for closeable, to open X closeable and to close them (written before Java7 autocloseable)
- Encoding utils: All encoding (BOM, String and Charset)
- File CRC32 utils: To get CRC32 from a file
- File size utils: List all file sizes from octects to exabioctets
- File system utils: All methods to manage files and directories
- File utils: To read, write and compare files
- Stream utils: To manage stream files (related to Input/OutputStream)

# utils-log
- MDCMT: To add easily MDC in your application in multi-threaded context

# utils-aop
- AbstractAspect: Base to include AOP (logging and profiling) in an application

# TODO

IO: handle copy/move/delete progress
