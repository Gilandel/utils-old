# utils
Utility classes

Commons:
- Assert: Improved version of Assert provided by the Spring Team,
- Cast generics: To cast map / list / object (avoid the use of @SuppressWarnings
- Class utils: To get super classes or to get common super classes
- Collection utils: Add missing transform methods (in complement of CollectionUtils provided by Apache Team)
- Date utils: Extend DateUtils from Apache project, add methods to get date if null and getDate wrapper to secure date transfer
- Enum char: A list of ASCII characters and others with their unicode and HTML version
- Enum utils: Extend EnumUtils from Apache project, add methods to get null if empty name is used (avoid exception)
- Hex utils: To convert hexadecimal in bytes
- Number utils: Extend NumberUtils from Apache project, add methods to check number equality (float and double)
- String utils: Extend NumberUtils from Apache project, add methods to get default string if empty or null

IO:
- Closeable manager: A manager for closeable, to open X closeable and to close them (written before Java7 autocloseable)
- Encoding utils: All encoding (BOM, String and Charset)
- File CRC32 utils: To get CRC32 from a file
- File size utils: List all file sizes from octects to exabioctets
- File system utils: All methods to manage files and directories
- File utils: To read, write and compare files
- Stream utils: To manage stream files (unrelated with Java 8 Stream)

Listener: base classes to manage events (listenable / event / listener)

MDCTS: To add mdc in our application in thread safe mode

Over:
- AbstractOverComparable: Class to force the implementation of compareTo method
- AbstractOverObject: Class to force implementation of toString, equals and hashCode

[![Build Status](https://api.travis-ci.org/Gilandel/utils.svg)](https://api.travis-ci.org/Gilandel/utils)