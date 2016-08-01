# utils-assertor

## Summary

1. [Summary](#summary)
2. [Description](#description)
  1. [Structure](#structure)
  2. [Reset explanations](#reset-explanations)
  3. [Message (locale, arguments and parameters)](#message-locale-arguments-and-parameters)
3. [Output details](#output-details)
  1. [toThrow](#tothrow)
  2. [isOK](#isok)
  3. [getErrors](#geterrors)
4. [Available methods](#available-methods)
  1. [NOT](#not)
  2. [For all (Object, Boolean...)](#for-all-object-boolean)
    1. [isNull](#isnull)
    2. [isNotNull](#isnotnull)
    3. [isEqual](#isequal)
    4. [isNotEqual](#isnotequal)
    5. [isInstance](#isinstance)
    6. [isAssignableFrom](#isassignablefrom)
    7. [matches](#matches)
    8. [validates](#validates)
  3. [Boolean](#boolean)
    1. [isTrue](#istrue)
    2. [isFalse](#isfalse)
  4. [Number](#number)
    1. [isGT](#isgt)
    2. [isGTE](#isgte)
    3. [isLT](#islt)
    4. [isLTE](#islte)
  5. [CharSequence](#charsequence)
    1. [hasLength](#haslength)
    2. [isEmpty](#isempty)
    3. [isNotEmpty](#isnotempty)
    4. [isBlank](#isblank)
    5. [isNotBlank](#isnotblank)
    6. [contains](#contains)
    7. [startsWith](#startswith)
    8. [startsWithIgnoreCase](#startswithignorecase)
    9. [endsWith](#endswith)
    10. [endsWithIgnoreCase](#endswithignorecase)
    11. [matches](#matches-1)
    12. [find](#find)
  6. [Date & Calendar](#date-calendar)
    1. [isAround](#isaround)
    2. [isNotAround](#isnotaround)
    3. [isAfter](#isafter)
    4. [isAfterOrEquals](#isafterorequals)
    5. [isBefore](#isbefore)
    6. [isBeforeOrEquals](#isbeforeorequals)
  7. [Array](#array)
    1. [hasLength](#haslength-1)
    2. [isEmpty](#isempty-1)
    3. [isNotEmpty](#isnotempty-1)
    4. [contains](#contains-1)
    5. [containsAll](#containsall)
    6. [containsAny](#containsany)
  8. [Iterable](#iterable)
    1. [hasSize](#hassize)
    2. [isEmpty](#isempty-2)
    3. [isNotEmpty](#isnotempty-2)
    4. [contains](#contains-2)
    5. [containsAll](#containsall-1)
    6. [containsAny](#containsany-1)
  9. [Map](#map)
    1. [hasSize](#hassize-1)
    2. [isEmpty](#isempty-3)
    3. [isNotEmpty](#isnotempty-3)
    4. [contains](#contains-3)
    5. [containsAll](#containsall-2)
    6. [containsAny](#containsany-2)
  10. [Class](#class)
    1. [isAssignableFrom](#isassignablefrom-1)

## Description

This module allow to assert parameters.

For now it manage:
- Boolean
- CharSequence (String, StringBuilder...)
- Number (Byte, Short, Integer, Long, Float, Double, BigInteger, BigDecimal)
- Date & Calendar
- Array
- Iterable (Set, List...)
- Map
- Class
- Object (the other objects)

### Structure

All assertions start with 'Assertor.that(object)' and following the type of the object, some methods are available.

About structure, an assertion can be cut in three parts:
- The definition of what we check: Assertor.that(myObject))...
- The check: ...isNull().or().isInstance(Color.class)...
- The output: ...toThrow()

Multiples objects can be check in the same line:
```java
Assertor.that(object1).isNull().and(object2).isNotNull().toThrow();
Assertor.that(object1).isNull().or().not().isInstance(Color.class).or(object2).isEqual(object3).isOk();
```

Three outputs are available:
- toThrow: to throw an exception if assertion is false,
- isOk: to get the boolean result of the assertion,
- getErrors: to get the error message.

These three output methods are considerate as final.
So a clear of intermediate conditions is done.

### Reset explanations

To avoid this, the parameter 'reset' can be set to 'false' (default: true).
A about the cleared, only the checked value is kept, any intermediate check is cleared.
```java
AssertCharSequence<String> assertion = Assertor.that("text1");
assertion.isBlank().and("text2").isNotEmpty().isOK(); // returns false
assertion.isNotBlank().isOK(); // returns true (all assertions are cleared, for 'text1' and 'text2') 
assertion.isBlank().and("text2").isNotEmpty().isOK(false); // returns false (reset set to false)
assertion.isNotBlank().isOK(); // returns false (here the isNotBlank call is linked to the previous through the operator AND)
// the last line is equivalent to:
Assertor.that("text1").isBlank().and().isNotBlank().isOK(); // the second check "text2" is lost
```

### Message (locale, arguments and parameters)

In each method, that manages intermediate errors (isBlank, contains...) or final errors (toThrow...) a locale can be specified.
The locale can be used to manage number and date (see [String.format](http://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)).

Also parameters and arguments can be injected.
Parameters are the variables send to check or as method parameters.
Arguments are the message arguments.
```java
Assertor.that("text").hasLength(5, "Bad length '%1$d' expected '%2$d*' for word '%1$s*'", 4).getErrors();
// "text" is the first parameter
// 5 is the second parameter
// 4 is the first argument
// Message thrown -> "Bad length '4' expected '5' for word 'text'"
```

As the previous example demonstrates it, parameters can be injected.
The syntax is exactly the same as default [String.format](http://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html) arguments, just suffix it by the character asterisk/star '*'.

## Output details

### toThrow
Throw an exception if the assertion is false. Two ways to personalize the exception exist:
- a message:
	The message can be personalized via arguments injection and locale.
	Back-side the method String.format will be called with these arguments.
	Parameters can also be injected.
- an exception:
	An exception can be also used (default: IllegalArgumentException).

* Signatures:
	- toThrow()
	- toThrow(CharSequence message, Object... arguments)
	- toThrow(Locale locale, CharSequence message, Object... arguments)
	- toThrow(E exception)
	- toThrow(boolean reset)
	- toThrow(boolean reset, CharSequence message, Object... arguments)
	- toThrow(boolean reset, Locale locale, CharSequence message, Object... arguments)
	- toThrow(boolean reset, E exception)

* Examples:
```java
Assertor.that("").isNotBlank().toThrow() -> throw the default message 'the char sequence should be NOT null, NOT empty and NOT blank'
Assertor.that("").isNotBlank("The first name is invalid").toThrow() -> throw the personalized message 'The first name is invalid'

Assertor.that("").isNotBlank().toThrow("Invalid field") -> throw the personalized message 'Invalid field'
Assertor.that("").isNotBlank("The first name is invalid").toThrow("Invalid field") -> throw the personalized message 'Invalid field'

Assertor.that("").isNotBlank().toThrow(Locale.FRANCE, "Invalid field (%.2fms)", 2.356) -> throw the personalized message 'Invalid field (2,36ms)'
Assertor.that("").isNotBlank("The first name is invalid").toThrow(Locale.FRANCE, "Invalid field (%.2fms)", 2.356) -> throw the personalized message 'Invalid field (2,36ms)'

Assertor.that("").isNotBlank().toThrow(new IOException("Invalid data")); -> throw the personalized exception
Assertor.that("").isNotBlank(The first name is invalid").toThrow(new IOException("Invalid data")); -> throw the personalized exception
```

As explain at the end of the description section, the reset parameter can be set to 'false' through these methods.
```java
Assertor.that("").isNotBlank().toThrow(false);
```

This mean:
```java
boolean reset = false;
Operator<AssertCharSequence<String>, String> operator = Assertor.that("").isNotBlank();
if (!operator.isOk(reset)) {
	LOGGER.error(operator.getErrors(reset));
	operator.toThrow(); // only here the assertion is cleared
	// if we catch exception, and retry at this point 'operator.toThrow()', no exception will thrown
}
```

### isOK
This method returns if the assertion is valid.

* Signatures:
	- isOk()
	- isOK(boolean reset)

* Examples:
```java
	Assertor.that("").isNotBlank().isOK() -> return false
	Assertor.that("").isBlank("The first name is invalid").isOK() -> return true
```

At the call of 'isOK()', the assertion is cleared, to avoid this, the parameter 'reset' can be set to 'false' (default: true).

### getErrors
This method returns the assertion errors.

* Signatures:
	- getErrors()
	- getErrors(boolean reset)

* Examples:
```java
	Assertor.that("").isNotBlank().getErrors() -> return "the char sequence should be NOT null, NOT empty and NOT blank"
	Assertor.that("").isBlank("The first name is invalid").isOK() -> return ""
```

At the call of 'getErrors()', the assertion is cleared, to avoid this, the parameter 'reset' can be set to 'false' (default: true).

## Available methods

### NOT
The not function is here to negate the next method (can be applied on any method, prerequisites are checked any way).

* Signatures:
	- not()

* Prerequisites: None

* Examples:
```java
Assertor.that(object).not().isNull().toThrow(); // become isNotNull
Assertor.that(strings).not().contains("text").toThrow(); // become does not contain
Assertor.that("text").not().hasLength(5, Locale.US, "The parameter '%s*' cannot be filled").toThrow(); // become has not length = 5
```

### For all (Object, Boolean...)

#### isNull
Assert that the object is null.

* Signatures:
	- isNull()
	- isNull(CharSequence message, Object[] arguments)
	- isNull(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isNull().toThrow();
Assertor.that(object).isNull("Cannot be filled").toThrow();
Assertor.that(object).isNull(Locale.US, "The parameter '%s*' cannot be filled").toThrow();
```

#### isNotNull
Assert that the object is NOT null.

* Signatures:
	- isNotNull()
	- isNotNull(CharSequence message, Object[] arguments)
	- isNotNull(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isNotNull().toThrow();
Assertor.that(name).isNotNull("Name cannot be null").toThrow();
```

#### isEqual
Assert that the object is equal to another.

* Signatures:
	- isEqual(Object object)
	- isEqual(Object object, CharSequence message, Object[] arguments)
	- isEqual(Object object, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isEqual(object2).toThrow();
Assertor.that(name).isEqual(object2, "Name '%s*' is not equal to '%s*'").toThrow();
```

* Info
This method is override by each other object types if necessary (like Date, Number).

#### isNotEqual
Assert that the object is NOT equal to another.

* Signatures:
	- isNotEqual(Object object)
	- isNotEqual(Object object, CharSequence message, Object[] arguments)
	- isNotEqual(Object object, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isNotEqual(object2).toThrow();
Assertor.that(name).isNotEqual(object2, "Name '%s*' already exists").toThrow();
```

* Info
This method is override by each other object types if necessary (like Date, Number).

#### isInstance
Assert that the object is an instance of the specified class.

* Signatures:
	- isInstance(Class class)
	- isInstance(Class class, CharSequence message, Object[] arguments)
	- isInstance(Class class, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- checked object NOT null
	- class NOT null

* Examples:
```java
Assertor.that(object).isInstance(class1).toThrow();
Assertor.that(object).isInstance(class1, "Input is not an instance of the class '%2$s*'").toThrow();
```

#### isAssignableFrom
Assert that the object is assignable from the specified class.

* Signatures:
	- isAssignableFrom(Class class)
	- isAssignableFrom(Class class, CharSequence message, Object[] arguments)
	- isAssignableFrom(Class class, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- checked object NOT null
	- class NOT null

* Examples:
```java
Assertor.that(object).isAssignableFrom(class1).toThrow();
Assertor.that(object).isAssignableFrom(class1, "Input is not assignable from the class '%2$s*'").toThrow();
```

#### matches
Assert that the object matches the hamcrest matcher.

* Signatures:
	- matches(org.hamcrest.Matcher matcher)
	- matches(org.hamcrest.Matcher matcher, CharSequence message, Object[] arguments)
	- matches(org.hamcrest.Matcher matcher, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- matcher NOT null

* Examples:
```java
Assertor.that(colors).matches(Matchers.hasSize(colors.size() - 1)).toThrow();
Assertor.that(colors).matches(Matchers.hasSize(colors.size() - 1), "Not the good color numbers").toThrow();
```

#### validates
Assert that the object validates the predicate.

* Signatures:
	- validates(PredicateThrowable<T, E> predicate)
	- validates(PredicateThrowable<T, E> predicate, CharSequence message, Object[] arguments)
	- validates(PredicateThrowable<T, E> predicate, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- predicate NOT null

* Examples:
```java
Assertor.that(object).validates((o) -> o != null).toThrow();
Assertor.that(object).validates((o) -> o != null, "The object is invalid").toThrow();
Assertor.that(object).validates((Object obj) -> {
    return obj != null;
}, Locale.US, "Object is null!!!").toThrow();

Assertor.that("/var/log/dev.log").validates((path) -> {
    return Paths.get(path).endsWith("dev.log");
}, Locale.US, "Path is invalid").isOK();
```

### Boolean

#### isTrue
Assert that the boolean is true.

* Signatures:
	- isTrue()
	- isTrue(CharSequence message, Object[] arguments)
	- isTrue(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that(bool).isTrue().toThrow();
Assertor.that(false).isTrue("Bad status").toThrow();
```

#### isFalse
Assert that the boolean is false.

* Signatures:
	- isFalse()
	- isFalse(CharSequence message, Object[] arguments)
	- isFalse(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that(bool).isFalse().toThrow();
Assertor.that(false).isFalse("Bad status").toThrow();
```

### Number
#### isGT
#### isGTE
#### isLT
#### isLTE

### CharSequence
#### hasLength
#### isEmpty
#### isNotEmpty
#### isBlank
#### isNotBlank
#### contains
#### startsWith
#### startsWithIgnoreCase
#### endsWith
#### endsWithIgnoreCase
#### matches
#### find

### Date & Calendar
#### isAround
#### isNotAround
#### isAfter
#### isAfterOrEquals
#### isBefore
#### isBeforeOrEquals

### Array
#### hasLength
#### isEmpty
#### isNotEmpty
#### contains
#### containsAll
#### containsAny

### Iterable
#### hasSize
#### isEmpty
#### isNotEmpty
#### contains
#### containsAll
#### containsAny

### Map
#### hasSize
#### isEmpty
#### isNotEmpty
#### contains
#### containsAll
#### containsAny

### Class
#### isAssignableFrom