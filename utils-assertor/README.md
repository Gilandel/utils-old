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
4. [Operators](#operators)
  1. [NOT](#not)
  2. [AND](#and)
  3. [OR](#or)
  4. [XOR](#xor)
5. [Available methods](#available-methods)
  1. [For all (Object, Boolean...)](#for-all-object-boolean)
    1. [isNull](#isnull)
    2. [isNotNull](#isnotnull)
    3. [isEqual](#isequal)
    4. [isNotEqual](#isnotequal)
    5. [isInstance](#isinstance)
    6. [isAssignableFrom](#isassignablefrom)
    7. [matches](#matches)
    8. [validates](#validates)
  2. [Boolean](#boolean)
    1. [isTrue](#istrue)
    2. [isFalse](#isfalse)
  3. [Number](#number)
    1. [isGT](#isgt)
    2. [isGTE](#isgte)
    3. [isLT](#islt)
    4. [isLTE](#islte)
  4. [CharSequence](#charsequence)
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
  5. [Date & Calendar](#date-calendar)
    1. [isAround](#isaround)
    2. [isNotAround](#isnotaround)
    3. [isAfter](#isafter)
    4. [isAfterOrEquals](#isafterorequals)
    5. [isBefore](#isbefore)
    6. [isBeforeOrEquals](#isbeforeorequals)
  6. [Array](#array)
    1. [hasLength](#haslength-1)
    2. [isEmpty](#isempty-1)
    3. [isNotEmpty](#isnotempty-1)
    4. [contains](#contains-1)
    5. [containsAll](#containsall)
    6. [containsAny](#containsany)
  7. [Iterable](#iterable)
    1. [hasSize](#hassize)
    2. [isEmpty](#isempty-2)
    3. [isNotEmpty](#isnotempty-2)
    4. [contains](#contains-2)
    5. [containsAll](#containsall-1)
    6. [containsAny](#containsany-1)
  8. [Map](#map)
    1. [hasSize](#hassize-1)
    2. [isEmpty](#isempty-3)
    3. [isNotEmpty](#isnotempty-3)
    4. [contains](#contains-3)
    5. [containsAll](#containsall-2)
    6. [containsAny](#containsany-2)
  9. [Class](#class)
    1. [isAssignableFrom](#isassignablefrom-1)
6. [Others](#others)
  1. [Expect](#expect)
7. [TODO](#todo)

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
- Object (all other objects)

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
So when these methods are called a clear of intermediate conditions is done.

### Reset explanations

Like explain in the previous chapter, to avoid the clearing of intermediate steps, the parameter 'reset' can be set to 'false' (default: true).
A about the clearing, only the checked value is kept, any intermediate checks are cleared.
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
Parameters are all the variables and other parameters sent to be used during the check.
Arguments are the message arguments.
```java
String text = "text";
...
Assertor.that(text).hasLength(5, "Bad length: '%1$d', expected: '%2$d*', text: '%1$s*'", text.length()).getErrors();
// "text" is the first parameter
// 5 is the second parameter
// 4 is the first argument
// Message thrown -> "Bad length '4' expected '5' for word 'text'"
```

As the previous example demonstrates it, parameters can be injected.
The syntax is exactly the same as default [String.format](http://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html) arguments, just suffix it by the character asterisk/star '*'.

## Output details

### toThrow
Throw an exception if the assertion is false. Three ways to personalize the exception exist:
- a message:
	The message can be personalized via arguments injection and locale.
	Back-side the method String.format will be called with these arguments.
	Parameters can also be injected.
- an exception:
	An exception can be also used (default: IllegalArgumentException).
- a function:
	A bi-function (a function with two parameters) can be passed.
	This function is only called if statement is false.
	The two parameters received are the combined errors messages and all the parameters.

* Signatures:
	- toThrow()
	- toThrow(CharSequence message, Object... arguments)
	- toThrow(Locale locale, CharSequence message, Object... arguments)
	- toThrow(E exception)
	- toThrow(BiFunction<CharSequence, Object[], E> exceptionBuilder)
	- toThrow(boolean reset)
	- toThrow(boolean reset, CharSequence message, Object... arguments)
	- toThrow(boolean reset, Locale locale, CharSequence message, Object... arguments)
	- toThrow(boolean reset, E exception)
	- toThrow(boolean reset, BiFunction<CharSequence, Object[], E> exceptionBuilder)

* Examples:
```java
Assertor.that("").isNotBlank().toThrow(); // -> throw the default message 'the char sequence should be NOT null, NOT empty and NOT blank'
Assertor.that("").isNotBlank("The first name is invalid").toThrow(); // -> throw the personalized message 'The first name is invalid'

Assertor.that("").isNotBlank().toThrow("Invalid field"); // -> throw the personalized message 'Invalid field'
Assertor.that("").isNotBlank("The first name is invalid").toThrow("Invalid field"); // -> throw the personalized message 'Invalid field'

Assertor.that("").isNotBlank().toThrow(Locale.FRANCE, "Invalid field (%.2fms)", 2.356); // -> throw the personalized message 'Invalid field (2,36ms)'
Assertor.that("").isNotBlank("The first name is invalid").toThrow(Locale.FRANCE, "Invalid field (%.2fms)", 2.356); // -> throw the personalized message 'Invalid field (2,36ms)'

Assertor.that("").isNotBlank().toThrow(new IOException("Invalid data")); // -> throw the personalized exception
Assertor.that("").isNotBlank(The first name is invalid").toThrow(new IOException("Invalid data")); -> throw the personalized exception

Assertor.that("text").isBlank().toThrow((errors, parameters) -> new MyException("text should be blank")); // -> throw a MyException with message: text should be blank
// 'errors' contains: the char sequence 'text' should be null, empty or blank
// 'parameters' contains: [{"text", EnumType.CHAR_SEQUENCE}]

Assertor.that("texte11").isBlank().or("texte12").not().startsWith("text").or().isBlank().toThrow((errors, parameters) -> new MyException(errors)); // -> throw a MyException
// 'errors' contains: the char sequence 'texte11' should be null, empty or blank OR the char sequence 'texte12' should NOT start with 'text'" OR the char sequence 'texte12' should be null, empty or blank
// 'parameters' contains: [{"texte11", EnumType.CHAR_SEQUENCE}, {"texte12", EnumType.CHAR_SEQUENCE}, {"text", EnumType.CHAR_SEQUENCE}]
// to display the first parameter in MyException call: parameters.get(0).getKey()
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
	// if we catch exception, and retry at this point 'operator.toThrow()', no exception will be thrown
}
```

### isOK
This method returns if the assertion is valid.

* Signatures:
	- isOk()
	- isOK(boolean reset)

* Examples:
```java
	Assertor.that("").isNotBlank().isOK(); // -> return false
	Assertor.that("").isBlank("The first name is invalid").isOK(); // -> return true
```

At the call of 'isOK()', the assertion is cleared, to avoid this, the parameter 'reset' can be set to 'false' (default: true).

### getErrors
This method returns the assertion errors.

* Signatures:
	- getErrors()
	- getErrors(boolean reset)

* Examples:
```java
	Assertor.that("").isNotBlank().getErrors(); // -> return "the char sequence should be NOT null, NOT empty and NOT blank"
	Assertor.that("").isBlank("The first name is invalid").getErrors(); // -> return ""
```

At the call of 'getErrors()', the assertion is cleared, to avoid this, the parameter 'reset' can be set to 'false' (default: true).

## Operators

### NOT
The 'not' function is here to negate the next method (can be applied on any method, prerequisites are checked any way).

* Signatures:
	- not()

* Prerequisites: None

* Examples:
```java
Assertor.that(object).not().isNull().toThrow(); // become isNotNull
Assertor.that(strings).not().contains("text").toThrow(); // become does not contain
Assertor.that("text").not().hasLength(5, Locale.US, "The parameter '%s*' cannot be filled").toThrow(); // become has not length = 5
```

### AND
The 'and' function is here to combine the previous and the next methods with the operator 'and'.
With a parameter, 'and' creates an sub assertor for the specified parameter.

* Signatures:
	- and()
	- and(Object object)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isNull().and().isInstance(MyClass.class).toThrow(); // is null or is and instance of MyClass
Assertor.that(12).iGT(12).and("text").contains("ex").toThrow(); // 12 > 12 and 'text' contains 'ex'
```

### OR
The 'or' function is here to combine the previous and the next methods with the operator 'or'.
With a parameter, 'or' creates an sub assertor for the specified parameter.

* Signatures:
	- or()
	- or(Object object)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isNull().or().isInstance(MyClass.class).toThrow(); // is null or is an instance of MyClass
Assertor.that(12).iGT(12).or("text").contains("ex").toThrow(); // 12 > 12 or 'text' contains 'ex'
```

### XOR
The 'xor' function is here to combine the previous and the next methods with the operator 'xor'.
With a parameter, 'xor' creates an sub assertor for the specified parameter.

* Signatures:
	- xor()
	- xor(Object object)

* Prerequisites: None

* Examples:
```java
Assertor.that(object).isNull().xor().isInstance(MyClass.class).toThrow(); // is null xor is an instance of MyClass
Assertor.that(12).iGT(12).xor("text").contains("ex").toThrow(); // 12 > 12 xor 'text' contains 'ex'
```

## Available methods

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

// prerequisite errors
Assertor.that(null).isInstance(class1).toThrow(); // -> throw an exception
Assertor.that(object).isInstance(null).toThrow(); // -> throw an exception
Assertor.that(null).not().isInstance(class1).toThrow(); // -> throw an exception
Assertor.that(object).not().isInstance(null).toThrow(); // -> throw an exception
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

// prerequisite errors
Assertor.that(null).isAssignableFrom(class1).toThrow(); // -> throw an exception
Assertor.that(object).isAssignableFrom(null).toThrow(); // -> throw an exception
Assertor.that(null).not().isAssignableFrom(class1).toThrow(); // -> throw an exception
Assertor.that(object).not().isAssignableFrom(null).toThrow(); // -> throw an exception
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

// prerequisite errors
Assertor.that(colors).matches(null).toThrow(); // -> throw an exception
Assertor.that(colors).not().matches(null).toThrow(); // -> throw an exception
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

// prerequisite errors
Assertor.that(object).validates(null).toThrow(); // -> throw an exception
Assertor.that(object).not().validates(null).toThrow(); // -> throw an exception
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
Assertor.that(bool).isTrue().toThrow(); // -> throw an exception, if bool == true
Assertor.that(false).isTrue("Bad status").toThrow(); // -> OK
Assertor.that(true).not().isTrue("Bad status").toThrow(); // -> OK
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
Assertor.that(bool).isFalse().toThrow(); // -> throw an exception, if bool == true
Assertor.that(false).isFalse("Bad status").toThrow(); // -> OK
Assertor.that(true).not().isFalse("Bad status").toThrow(); // -> OK
```

### Number
#### isGT
Assert that number is greater than specified number.

* Signatures:
	- isGT(Number number)
	- isGT(Number number, CharSequence message, Object[] arguments)
	- isGT(Number number, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- both number NOT null

* Examples:
```java
Assertor.that(12).isGT(12).toThrow(); // -> throw an exception
Assertor.that(12).isGT(10, "Bad status").toThrow(); // -> OK
Assertor.that(12).not().isGT(12).toThrow(); // -> OK

// prerequisite errors
Assertor.that(null).isGT(12).toThrow(); // -> throw an exception
Assertor.that(12).isGT(null).toThrow(); // -> throw an exception
Assertor.that(null).not().isGT(12).toThrow(); // -> throw an exception
Assertor.that(12).not().isGT(null).toThrow(); // -> throw an exception
```

#### isGTE
Assert that number is greater than or equal to specified number.

* Signatures:
	- isGTE(Number number)
	- isGTE(Number number, CharSequence message, Object[] arguments)
	- isGTE(Number number, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- both number NOT null

* Examples:
```java
Assertor.that(12).isGTE(13).toThrow(); // -> throw an exception
Assertor.that(12).isGTE(12).toThrow(); // -> OK
Assertor.that(12).isGTE(10, "Bad status").toThrow(); // -> OK
Assertor.that(12).not().isGTE(13).toThrow(); // -> OK

// prerequisite errors
Assertor.that(null).isGTE(12).toThrow(); // -> throw an exception
Assertor.that(12).isGTE(null).toThrow(); // -> throw an exception
Assertor.that(null).not().isGTE(12).toThrow(); // -> throw an exception
Assertor.that(12).not().isGTE(null).toThrow(); // -> throw an exception
```

#### isLT
Assert that number is lower than specified number.

* Signatures:
	- isLT(Number number)
	- isLT(Number number, CharSequence message, Object[] arguments)
	- isLT(Number number, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- both number NOT null

* Examples:
```java
Assertor.that(12).isLT(12).toThrow(); // -> throw an exception
Assertor.that(12).isLT(13, "Bad status").toThrow(); // -> OK
Assertor.that(12).not().isLT(12).toThrow(); // -> OK

// prerequisite errors
Assertor.that(null).isLT(12).toThrow(); // -> throw an exception
Assertor.that(12).isLT(null).toThrow(); // -> throw an exception
Assertor.that(null).not().isLT(12).toThrow(); // -> throw an exception
Assertor.that(12).not().isLT(null).toThrow(); // -> throw an exception
```

#### isLTE
Assert that number is lower than or equal to specified number.

* Signatures:
	- isLTE(Number number)
	- isLTE(Number number, CharSequence message, Object[] arguments)
	- isLTE(Number number, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- both number NOT null

* Examples:
```java
Assertor.that(12).isLTE(11).toThrow(); // -> throw an exception
Assertor.that(12).isLTE(12).toThrow(); // -> OK
Assertor.that(12).isLTE(13, "Bad status").toThrow(); // -> OK
Assertor.that(12).not().isLTE(11).toThrow(); // -> OK

// prerequisite errors
Assertor.that(null).isLTE(12).toThrow(); // -> throw an exception
Assertor.that(12).isLTE(null).toThrow(); // -> throw an exception
Assertor.that(null).not().isLTE(12).toThrow(); // -> throw an exception
Assertor.that(12).not().isLTE(null).toThrow(); // -> throw an exception
```

### CharSequence
#### hasLength
Assert that char sequence has the specified length.

* Signatures:
	- hasLength(int length)
	- hasLength(int length, CharSequence message, Object[] arguments)
	- hasLength(int length, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- char sequence NOT null
	- length >= 0

* Examples:
```java
Assertor.that("text").hasLength(3).toThrow(); // -> throw an exception
Assertor.that("text").hasLength(4, "Bad status").toThrow(); // -> OK
Assertor.that("text").not().hasLength(3).toThrow(); // -> OK

// prerequisite errors
Assertor.that(null).hasLength(4, "Bad status").toThrow(); // -> throw an exception
Assertor.that("text").hasLength(-1, "Bad status").toThrow(); // -> throw an exception
Assertor.that(null).not().hasLength(4, "Bad status").toThrow(); // -> throw an exception
Assertor.that("text").not().hasLength(-1, "Bad status").toThrow(); // -> throw an exception
```

#### isEmpty
Assert that char sequence is empty or null.

* Signatures:
	- isEmpty()
	- isEmpty(CharSequence message, Object[] arguments)
	- isEmpty(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that("text").isEmpty().toThrow(); // -> throw an exception
Assertor.that((CharSequence) null).isEmpty("Param '%1$s*' not empty").toThrow(); // -> OK
Assertor.that("").isEmpty("Param '%1$s*' not empty").toThrow(); // -> OK
Assertor.that("text").not().isEmpty("Param '%1$s*' not empty").toThrow(); // -> OK
```

#### isNotEmpty
Assert that char sequence is NOT empty and NOT null.

* Signatures:
	- isNotEmpty()
	- isNotEmpty(CharSequence message, Object[] arguments)
	- isNotEmpty(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that((CharSequence) null).isNotEmpty().toThrow(); // -> throw an exception
Assertor.that("").isNotEmpty().toThrow(); // -> throw an exception
Assertor.that("text").isNotEmpty("Param '%1$s*' empty or null").toThrow(); // -> OK

```

#### isBlank
Assert that char sequence is blank or empty or null.

* Signatures:
	- isBlank()
	- isBlank(CharSequence message, Object[] arguments)
	- isBlank(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that("text").isBlank().toThrow(); // -> throw an exception
Assertor.that(null).isBlank("Param '%1$s*' not blank").toThrow(); // -> OK
Assertor.that("").isBlank("Param '%1$s*' not blank").toThrow(); // -> OK
Assertor.that("   ").isBlank("Param '%1$s*' not blank").toThrow(); // -> OK
Assertor.that("text").not().isBlank("Param '%1$s*' not blank").toThrow(); // -> OK
```

#### isNotBlank
Assert that char sequence is NOT blank and NOT empty and NOT null.

* Signatures:
	- isNotBlank()
	- isNotBlank(CharSequence message, Object[] arguments)
	- isNotBlank(Locale locale, CharSequence message, Object[] arguments)

* Prerequisites: None

* Examples:
```java
Assertor.that("text").isNotBlank().toThrow(); // -> OK
Assertor.that("text").isNotBlank("Param '%1$s*' not blank").toThrow(); // -> OK
Assertor.that("text").isNotBlank().toThrow(); // -> OK
Assertor.that(null).isNotBlank("Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that("").isNotBlank("Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that("   ").isNotBlank("Param '%1$s*' not blank").toThrow(); // -> throw an exception
```

#### contains
Assert that char sequence contains the substring.

* Signatures:
	- contains(CharSequence substring)
	- contains(CharSequence substring, CharSequence message, Object[] arguments)
	- contains(CharSequence substring, Locale locale, CharSequence message, Object[] arguments)

* Prerequisites:
	- char sequence NOT null
	- substring NOT null and NOT empty

* Examples:
```java
Assertor.that("text").contains("t").toThrow(); // -> OK
Assertor.that("text").contains("ex", "Param '%1$s*' not blank").toThrow(); // -> OK
Assertor.that("text").contains("text").toThrow(); // -> OK
Assertor.that("text").contains("y").toThrow(); // -> throw an exception
Assertor.that("text").not().contains("y").toThrow(); // -> OK

// prerequisite errors
Assertor.that(null).contains("t", "Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that("text").contains(null, "Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that("text").contains("", "Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that(null).not().contains("t", "Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that("text").not().contains(null, "Param '%1$s*' not blank").toThrow(); // -> throw an exception
Assertor.that("text").not().contains("", "Param '%1$s*' not blank").toThrow(); // -> throw an exception
```

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

## Others
### Expect
Validate a thrown exception and its message.

* Signatures:
	- exception(AssertConsumer<Throwable> consumer, Class<? extends Throwable> expectedException)
	- exception(AssertConsumer<Throwable> consumer, Class<? extends Throwable> expectedException, String expectedMessage)
	- exception(AssertConsumer<Throwable> consumer, Class<? extends Throwable> expectedException, final TriFunction<Boolean, String, String, E> exceptionFunction)
	- exception(AssertConsumer<Throwable> consumer, Class<? extends Throwable> expectedException, String expectedMessage, final TriFunction<Boolean, String, String, E> exceptionFunction)

* Parameters:
	- consumer: The consumer or where the code to checked can be placed
	- expectedException: The class of expected exception
	- expectedMessage: The expected exception message
	- exceptionFunction: The exception builder, only called on mismatch. Has 3 parameters:
		- first: true, if the expected exception matches
		- second: the expected message
		- third: the actual message

* Prerequisites:
	- consumer NOT null
	- expectedException NOT null

* Examples:
```java
Expect.exception(() -> {
    // throw new IllegalArgumentException("parameter cannot be null");
    getMyType(null);
}, IllegalArgumentException.class); // -> OK

Expect.exception(() -> {
    // throw new IOException("parameter cannot be null");
    getMyType(null);
}, IllegalArgumentException.class); // -> throw an ExpectException



Expect.exception(() -> {
     // throw new IllegalArgumentException("parameter cannot be null");
     getMyType(null);
 }, IllegalArgumentException.class, "parameter cannot be null"); // -> OK
 
 Expect.exception(() -> {
     // throw new IllegalArgumentException("type cannot be null");
     getMyType(null);
 }, IllegalArgumentException.class, "parameter cannot be null");  // -> throw an ExpectException



// Obviously, you can save this in a static variable to share it.
// This function is not provided by module to avoid a JUnit dependency.
// ComparisonFailure come from: org.junit.ComparisonFailure

TriFunction<Boolean, String, String> junitError = (catched, expected, actual) -> {
    if (catched) {
        return new ComparisonFailure("The exception message don't match.", expected, actual);
    } else {
        return new AssertionError("The expected exception never came up");
    }
};



Expect.exception(() -> {
    // throw new IllegalArgumentException("parameter cannot be null");
    getMyType(null);
}, IllegalArgumentException.class, junitError); // -> OK

Expect.exception(() -> {
    // throw new IOException("parameter cannot be null");
    getMyType(null);
}, IllegalArgumentException.class, junitError); // -> throw an AssertionError


Expect.exception(() -> {
     // throw new IllegalArgumentException("parameter cannot be null");
     getMyType(null);
 }, IllegalArgumentException.class, "parameter cannot be null", junitError); // -> OK
 
 Expect.exception(() -> {
     // throw new IllegalArgumentException("type cannot be null");
     getMyType(null);
 }, IllegalArgumentException.class, "parameter cannot be null", junitError);  // -> throw a ComparisonFailure
```

## TODO

- Build all messages in one step at the end (one call to String.format, which locale, if multiple?)