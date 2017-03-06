# utils-commons

##Summary

1. [Summary](#summary)
2. [Commons](#commons)
  1. [Default](#default)
  2. [Result](#result)
3. [Builder](#builder)
  1. [EqualsBuilder](#equalsbuilder)
  2. [EqualsBuilder2](#equalsbuilder2)
  3. [HashCodeBuilder](#hashcodebuilder)
  4. [ToStringBuilder](#tostringbuilder)
4. [Exception](#exception)
5. [Function](#function)
6. [Listener](#listener)
7. [Over](#over)
8. [Tuple](#tuple)

##Commons
- ArrayUtils: Extend ArrayUtils from Apache project, add methods to check array,
- CastUtils: To cast map / list / object into typed objects,
- ClassUtils: To get super classes or to get common super classes,
- CollectionUtils2: Add missing transform methods (in complement of CollectionUtils provided by Apache Team),
- Comparators: Basics comparators (Byte, Short, Long, Float, Double, BigInteger BigDecimal, Char, String, Date, Calendar, Duration, Instant, OffsetTime, OffsetDateTime, LocalTime, LocalDateTime, ZonedDateTime) and Maven version comparator,
- DateUtils: Extend DateUtils from Apache project, add methods to get date if null, getDate wrapper to secure date transfer and between to calculate time between dates,
- Default: A class like Optional, but if a null value is set or is defined as empty, this method returns the default value,
- EnumChar: A list of ASCII characters and others with their unicode and HTML version,
- EnumUtils: Extend EnumUtils from Apache project, add methods to get null if empty name is used (to avoid exception)
- HexUtils: To convert hexadecimal in bytes,
- NumberUtils: Extend NumberUtils from Apache project, add methods to check number equality,
- ObjectUtils: Extend ObjectUtils from Apache project, add features for Object,
- Result: A class like Optional, but if a null value is set (not empty), this method returns 'present', the aim is to differentiate an empty value and a null,
- StringUtils: Extend StringUtils from Apache project, add methods to get default string if empty or null.

###Default

```java
Default.empty(defaultText).get(); // => returns 'defaultText' content ('defaultText' cannot be null)
Default.empty(defaultText).isPresent(); // => returns 'false'
Default.empty(defaultText).ifPresent(consumer); // does nothing
Default.empty(defaultText).ifAbsent(consumer); // executes the consumer (on the default value)

Default.of(text).get(); // => returns 'text' content ('text' cannot be null)
Default.of(text).isPresent(); // => returns 'true'
Default.of(text).ifPresent(consumer); // executes the consumer (on the value)
Default.of(text).ifAbsent(consumer); // does nothing

Default.ofNullable(text, defaultText).get();
// => if 'text' is not null, returns 'text' content, otherwise returns 'defaultText' content ('text' may be null, 'defaultText' cannot be null)
Default.ofNullable(text, defaultText).isPresent(); // => returns 'true' or 'false'
Default.ofNullable(text, defaultText).ifPresent(consumer); // executes the consumer if 'text' is not null (on the value)
Default.ofNullable(text, defaultText).ifAbsent(consumer); // executes the consumer if 'text' is null (on the default value)
```

###Result

```java
Result.empty().isPresent(); // => returns 'false'
Result.empty().isNull(); // => returns 'true'
Result.empty().ifPresent(consumer); // does nothing
Result.empty().ifNotNull(consumer); // does nothing

Result.of(text).isPresent(); // => returns 'true' ('text' cannot be null)
Result.of(text).isNull(); // => returns 'false'
Result.of(text).ifPresent(consumer); // executes the consumer
Result.of(text).ifNotNull(consumer); // executes the consumer

Result.ofNullable(text).isPresent(); // => returns 'true' ('text' may be null)
Result.ofNullable(text).isNull(); // => returns 'true' or 'false'
Result.ofNullable(text).ifPresent(consumer); // executes the consumer
Result.ofNullable(text).ifNotNull(consumer); // executes the consumer if 'text' is not null
```

##Builder
- EqualsBuilder: Extend EqualsBuilder from Apache project, add the possibility to append a property through a functional getter function,
- EqualsBuilder2: Based on the fact that the most of the time I compare DTOs, the class provide a constructor for both checked objects and appenders based on these objects with the ability to check them througth functional getters and predicates.
- HashCodeBuilder: Extend EqualsBuilder from Apache project, add the possibility to append a property through a functional getter function,
- ToStringBuilder: Another version of the ToStringBuilder, simple and also faster.

###EqualsBuilder

```java
new EqualsBuilder()
	.append(dto1, dto2, dto -> dto.getId())
	.isEqual();
```

###EqualsBuilder2

```java
// To check 2 DTOs with 2 properties (id and name)
// Here we check, if both ids are equals (or both null)
// We also check if both names are equals (ignoring case consideration),
// previous calling the predicate function, a null check is automatically performed to avoid NullPointerException

new EqualsBuilder2<>(dto1, dto2)
	.append(dto -> dto.getId())
	.append(dto -> dto.getName(), (name1, name2) -> name1.equalsIgnoreCase(name2))
	.isEqual();

// dto1 = null, dto2 = {id:1, name:TesT} => false
// dto1 = {id:1, name:test}, dto2 = null => false
// dto1 = {id:1, name:test}, dto2 = {id:1, name:TesT} => true
// dto1 = {id:1, name:test}, dto2 = {id:2, name:TesT} => false
// dto1 = {id:null, name:test}, dto2 = {id:1, name:TesT} => false
// dto1 = {id:null, name:test}, dto2 = {id:null, name:TesT} => true
// dto1 = {id:null, name:null}, dto2 = {id:null, name:TesT} => false
// dto1 = {id:null, name:null}, dto2 = {id:1, name:null} => true

// Other example:

new EqualsBuilder2<>(dto1, dto2)
	.append(dto -> dto.getId())
	.append(dto -> dto.getName(), (name1, name2) -> name1.equalsIgnoreCase(name2))
	.and(dto1, dto3, CommonInterface.class)
	.append(dto -> dto.getDescription())
	.isEqual();

// In this case, the append on 'dto1' and 'dto3' is based on the common method defined in 'CommonInterface'

// If 'and' method is used to compare multiple objects, each step can be saved, the 'equals' is performed at the end.
// Indeed, only first checks are done at the beginning on objects (null, instance equals, comparable class), all appended checks are done on 'isEqual' call.

EqualsBuilder2<Dto1Type, Dto3Type> builder = new EqualsBuilder2<>(dto1, dto2)
	.append(dto -> dto.getId())
	.append(dto -> dto.getName(), (name1, name2) -> name1.equalsIgnoreCase(name2))
	.and(dto1, dto3, CommonInterface.class)
	.append(dto -> dto.getDescription());
	
dto1.setName(newName);

builder.isEqual(); // the equality will take account of the dto1 new name.
```

###HashCodeBuilder

```java
new HashCodeBuilder()
	.append(dto, dto -> dto.getId() * 37)
	.isEqual();
```

###ToStringBuilder

```java
new ToStringBuilder(this, ToStringStyle.JSON)
	.append("key", value)
	.append("key", () -> value)
	.append("key", () -> value, text -> text.toUpperCase())
	.append("key", 126452.2223, ToStringBuilder.FORMATTER)
	.appendIfPresent("key", Optional.ofNullable(null))
	.build();
// =&gt; {"org.test.MyClass":{"key":"value","key":"value","key":"VALUE","key":"126 452.222"}}
```

##Exception
- AbstractException: Base class for Exception (add constructors to directly create message with arguments),
- AbstractException: Base class for RuntimeException (add constructors to directly create message with arguments),
- FunctionException: Runtime Exception thrown on error with throwable functional interfaces,
- IllegalOperationException: Specific Runtime Exception used by functional interface in case of unauthorized operation (mainly in QueryBuilder).

##Function
Functional interfaces that support to throw exception:
- ThrowableSupplier: To supply an exception (just throws the given exception),
- *Throwable: Functional interfaces that manage one parameters,
- Bi*Throwable: Functional interfaces that manage two parameters,
- Tri*Throwable: Functional interfaces that manage three parameters,
- Quad*Throwable: Functional interfaces that manage four parameters.

Also, missing standard functional interfaces are provided to manage 2, 3 and 4 parameters.

The catched exceptions (for *Throwable interfaces) are mapped into FunctionException (an extend of RuntimeException).

```java
// definition
void myMethod(ThrowableSupplier<MyException> throwableSupplier);

// call the method
myMethod(() -> throw new MyException());

// definition
void myMethod(SupplierThrowable<String, MyException> supplier);

// call the method
myMethod(() -> Optional.ofNullable(myString).orElseThrow(new MyException("myString cannot be null")));
```

##Listener
Very simple classes to manage events (listenable / event / listener).

##Over
- AbstractOverComparable: Class to force the implementation of compareTo method
- AbstractOverObject: Class to force implementation of toString, equals and hashCode 

##Tuple
In addition of tuples provided by the Apache Team, this package provides new tuples to create container object for 1, 2, 3 and 4 parameters.
Main entry points to use this, are the classes with the following names:
- Single: To create a container for an object (interesting when you use a variable in and out a functional interface),
- PairIso: To create a Pair of the same type,
- TripleIso: To create a Triple of the same type,
- Quad: To create a Quad of four different types,
- QuadIso: To create a Quad of the same type,
- Generic: To create a container of unlimited objects (the only purpose is in a Proof Of Concept context to differentiate a future DTO (post POC) with a list).

```java
Single<String> single = Single.of("v1");
PairIso<String> pairIso = PairIso.of("v1", "v2");
TripleIso<String> tripleIso = PairIso.of("v1", "v2", "v3");
QuadIso<String> quadIso = PairIso.of("v1", "v2", "v3", "v4");
Quad<String, String, String, Integer> quad = Quad.of("v1", "v2", "v3", 12);
Generic<String> generic = Generic.of("v1", "v2", "v3", "v4", "v5", "v6", "v7"); // ...

// A simple use case for Single (how to set a variable in a consumer usable after)
Color c = Color.BLACK;
final Single<Color> colorContainer = Single.ofMutable(c);
checker.test(v -> colorContainer.set(v));
LOGGER.info("My new color: {}", colorContainer.get());
```