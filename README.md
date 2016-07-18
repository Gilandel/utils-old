# TODO
ecrire d'autres TU
ecrire mapper + include converter or supplier/consumer

gestion des list/list/DTO

gestion simple
mapper.map(object, class)
mapper.map(object, class, mode)
mapper.map(object, class, mode, deep)
gestion des converters par champ (function)
gestion des transformers par champ (function)
gestion multi niveau / multi type (DTO, IDO)
gestion du mapping par champ de nom différent
gestion de la mise en cache du mapping (reflection java) via scan
gestion de l'annotation et des types transient

dans le cas de collection non trie, exploiter le stream parallelise

nouvelles proprietes du mappable (@MappableProperty(name, constructor, comparator, mode, deep, converter, targetClass)
possibilite de typer les collections (@MappableProperty(constructor=TreeSet.class, comparator=Comparator) private Set<DTO> test;

support transverse Hibernate via Consumer ou  Function
support avec et sans spring

mettre a dispo des converters de date/String en utilisant les standards J8

mise en place de JMH

scripts loader multi config (replacer/loader) config commune geree par le loader
public static getValues()


# utils
Utility classes

[![Build Status](https://travis-ci.org/Gilandel/utils.svg?branch=develop)](https://travis-ci.org/Gilandel/utils)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/e34c82e78aaf45a797721e62a7a31a0a)](https://www.codacy.com/app/gilles/utils)
[![Dependency Status](https://www.versioneye.com/user/projects/571407adfcd19a00415b1a84/badge.svg?style=flat)](https://www.versioneye.com/user/projects/571407adfcd19a00415b1a84)
[![codecov.io](https://codecov.io/github/Gilandel/utils/coverage.svg?branch=develop)](https://codecov.io/gh/Gilandel/utils/branch/develop)

# utils-commons
##Commons:
- Assert utils: Improved version of Assert provided by the Spring Team,
- Cast generics: To cast map / list / object (avoid the use of @SuppressWarnings
- Class utils: To get super classes or to get common super classes
- Collection utils: Add missing transform methods (in complement of CollectionUtils provided by Apache Team)
- Date utils: Extend DateUtils from Apache project, add methods to get date if null and getDate wrapper to secure date transfer
- Enum char: A list of ASCII characters and others with their unicode and HTML version
- Enum utils: Extend EnumUtils from Apache project, add methods to get null if empty name is used (avoid exception)
- Hex utils: To convert hexadecimal in bytes
- Number utils: Extend NumberUtils from Apache project, add methods to check number equality
- String utils: Extend NumberUtils from Apache project, add methods to get default string if empty or null

##IO:
- Closeable manager: A manager for closeable, to open X closeable and to close them (written before Java7 autocloseable)
- Encoding utils: All encoding (BOM, String and Charset)
- File CRC32 utils: To get CRC32 from a file
- File size utils: List all file sizes from octects to exabioctets
- File system utils: All methods to manage files and directories
- File utils: To read, write and compare files
- Stream utils: To manage stream files (unrelated with Java 8 Stream)

##Listener:
Base classes to manage events (listenable / event / listener)

##MDCMT:
To add easily MDC in your application in multi-threaded context

##Over:
- AbstractOverComparable: Class to force the implementation of compareTo method
- AbstractOverObject: Class to force implementation of toString, equals and hashCode

# utils-aop
- AbstractAspect: Base to include AOP (logging and profiling) in an application