/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import java.util.Collection;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.StringDescription;

/**
 * Assertion utility class that assists in validating arguments.
 *
 * <p>
 * Useful for identifying programmer errors early and clearly at runtime.
 *
 * <p>
 * For example, if the contract of a public method states it does not allow
 * {@code null} arguments, {@code Assert} can be used to validate that contract.
 * Doing this clearly indicates a contract violation when it occurs and protects
 * the class's invariants.
 *
 * <p>
 * Typically used to validate method arguments rather than configuration
 * properties, to check for cases that are usually programmer errors rather than
 * configuration errors. In contrast to configuration initialization code, there
 * is usually no point in falling back to defaults in such methods.
 *
 * <p>
 * This class is similar to JUnit's assertion library. If an argument value is
 * deemed invalid, an {@link IllegalArgumentException} is thrown (typically).
 * For example:
 *
 * <pre class="code">
 * Assert.notNull(clazz, &quot;The class must not be null&quot;);
 * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
 * </pre>
 *
 * <p>
 * Mainly for internal use within the framework; consider
 * <a href="http://commons.apache.org/proper/commons-lang/">Apache's Commons
 * Lang</a> for a more comprehensive suite of {@code String} utilities.
 *
 * @see <a href=
 *      "http://docs.spring.io/spring/docs/2.0.x/api/org/springframework/util/Assert.html?is-external=true">
 *      org.springframework.util.Assert</a>
 *
 * @author Keith Donald
 * @author Juergen Hoeller
 * @author Colin Sampaleanu
 * @author Rob Harrop
 * @author Sam Brannen
 * @author Gilles Landel
 * @since 1.1.2
 */
public abstract class Assert {

    private static final String ASSERTION_FAILED = "[Assertion failed]";

    private static Locale locale = Locale.US;

    /**
     * 
     * Constructor
     *
     */
    public Assert() {
    }

    /**
     * @return the locale
     */
    public static final Locale getLocale() {
        return Assert.locale;
    }

    /**
     * @param locale
     *            the locale to set
     */
    public static final void setLocale(final Locale locale) {
        Assert.locale = locale;
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre class="code">
     * Assert.isFalse(i &gt; 0, &quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isFalse(final boolean expression, final String message, final Object... arguments) {
        if (expression) {
            throw new IllegalArgumentException(getMessage("this expression must be false", message, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre class="code">
     * Assert.isFalse(i &gt; 0);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isFalse(final boolean expression) {
        isFalse(expression, null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre class="code">
     * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isTrue(final boolean expression, final String message, final Object... arguments) {
        if (!expression) {
            throw new IllegalArgumentException(getMessage("this expression must be true", message, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre class="code">
     * Assert.isTrue(i &gt; 0);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isTrue(final boolean expression) {
        isTrue(expression, null);
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre class="code">
     * Assert.isNull(value, &quot;The value must be null&quot;);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public static void isNull(final Object object, final String message, final Object... arguments) {
        if (object != null) {
            throw new IllegalArgumentException(getMessage("the object argument must be null", message, arguments));
        }
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre class="code">
     * Assert.isNull(value);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public static void isNull(final Object object) {
        isNull(object, null);
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre class="code">
     * Assert.isNotNull(clazz, &quot;The class must not be null&quot;);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public static void isNotNull(final Object object, final String message, final Object... arguments) {
        if (object == null) {
            throw new IllegalArgumentException(getMessage("this argument is required; it must not be null", message, arguments));
        }
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre class="code">
     * Assert.isNotNull(object);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public static void isNotNull(final Object object) {
        isNotNull(object, null);
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isNotEmpty(final String text, final String message, final Object... arguments) {
        if (StringUtils.isEmpty(text)) {
            throw new IllegalArgumentException(
                    getMessage("this String argument must have length; it must not be null or empty", message, arguments));
        }
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(name);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isNotEmpty(final String text) {
        isNotEmpty(text, null);
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(array, &quot;The array must have elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public static void isNotEmpty(Object[] array, final String message, final Object... arguments) {
        if (ArrayUtils.isEmpty(array)) {
            throw new IllegalArgumentException(
                    getMessage("this array must not be empty: it must contain at least 1 element", message, arguments));
        }
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public static void isNotEmpty(Object[] array) {
        isNotEmpty(array, null);
    }

    /**
     * Assert that a collection has elements; that is, it must not be
     * {@code null} and must have at least one element.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(collection, &quot;Collection must have elements&quot;);
     * </pre>
     * 
     * @param collection
     *            the collection to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the collection is {@code null} or has no elements
     */
    public static void isNotEmpty(final Collection<?> collection, final String message, final Object... arguments) {
        if (CollectionUtils.isEmpty(collection)) {
            throw new IllegalArgumentException(
                    getMessage("this collection must not be empty: it must contain at least 1 element", message, arguments));
        }
    }

    /**
     * Assert that a collection has elements; that is, it must not be
     * {@code null} and must have at least one element.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(collection, &quot;Collection must have elements&quot;);
     * </pre>
     * 
     * @param collection
     *            the collection to check
     * @throws IllegalArgumentException
     *             if the collection is {@code null} or has no elements
     */
    public static void isNotEmpty(final Collection<?> collection) {
        isNotEmpty(collection, null);
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre class="code">
     * Assert.isNotEmpty(map, &quot;Map must have entries&quot;);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public static void isNotEmpty(final Map<?, ?> map, final String message, final Object... arguments) {
        if (MapUtils.isEmpty(map)) {
            throw new IllegalArgumentException(
                    getMessage("this map must not be empty; it must contain at least one entry", message, arguments));
        }
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre class="code">
     * Assert.notEmpty(map);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public static void isNotEmpty(final Map<?, ?> map) {
        isNotEmpty(map, null);
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre class="code">
     * Assert.isEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isEmpty(final String text, final String message, final Object... arguments) {
        if (StringUtils.isNotEmpty(text)) {
            throw new IllegalArgumentException(getMessage("this String argument must be null or empty", message, arguments));
        }
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre class="code">
     * Assert.isEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isEmpty(final String text) {
        isEmpty(text, null);
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre class="code">
     * Assert.isNotBlank(name, &quot;'name' must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isNotBlank(final String text, final String message, final Object... arguments) {
        if (StringUtils.isBlank(text)) {
            throw new IllegalArgumentException(
                    getMessage("this String argument must have text; it must not be null, empty, or blank", message, arguments));
        }
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre class="code">
     * Assert.isNotBlank(name, &quot;'name' must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isNotBlank(final String text) {
        isNotBlank(text, null);
    }

    /**
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre class="code">
     * Assert.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isBlank(final String text, final String message, final Object... arguments) {
        if (StringUtils.isNotBlank(text)) {
            throw new IllegalArgumentException(getMessage("this String argument must be null, empty or blank", message, arguments));
        }
    }

    /**
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre class="code">
     * Assert.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isBlank(final String text) {
        isBlank(text, null);
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre class="code">
     * Assert.contains(name, &quot;rod&quot;, &quot;Name must contain 'rod'&quot;);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public static void contains(final String textToSearch, final String substring, final String message, final Object... arguments) {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && !textToSearch.contains(substring)) {
            throw new IllegalArgumentException(
                    getMessage("this String argument must contain the substring [" + substring + "]", message, arguments));
        }
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre class="code">
     * Assert.contains(name, &quot;rod&quot;);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public static void contains(final String textToSearch, final String substring) {
        contains(textToSearch, substring, null);
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre class="code">
     * Assert.doesNotContain(name, &quot;rod&quot;, &quot;Name must not contain 'rod'&quot;);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public static void doesNotContain(final String textToSearch, final String substring, final String message, final Object... arguments) {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && textToSearch.contains(substring)) {
            throw new IllegalArgumentException(
                    getMessage("this String argument must not contain the substring [" + substring + "]", message, arguments));
        }
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre class="code">
     * Assert.doesNotContain(name, &quot;rod&quot;);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public static void doesNotContain(final String textToSearch, final String substring) {
        doesNotContain(textToSearch, substring, null);
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre class="code">
     * Assert.isNoNullElements(array, &quot;The array must have non-null elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void isNoNullElements(Object[] array, final String message, final Object... arguments) {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    throw new IllegalArgumentException(getMessage("this array must not contain any null elements", message, arguments));
                }
            }
        }
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre class="code">
     * Assert.isNoNullElements(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void isNoNullElements(Object[] array) {
        isNoNullElements(array, null);
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre class="code">
     * Assert.isNotEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public static void isNotEqual(final Object obj1, final Object obj2, final String message, final Object... arguments) {
        if (obj1 == null && obj2 == null) {
            throw new IllegalArgumentException(getMessage("Both objects are null.", message, arguments));
        } else if (obj1 != null && obj2 != null && obj1.equals(obj2)) {
            throw new IllegalArgumentException(getMessage("Object1 is equal to Object2.", message, arguments));
        }
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre class="code">
     * Assert.isNotEqual(foo1, foo2);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public static void isNotEqual(final Object obj1, final Object obj2) {
        Assert.isNotEqual(obj1, obj2, null);
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre class="code">
     * Assert.isEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and not the other one or
     *             are not equal.
     */
    public static void isEqual(final Object obj1, final Object obj2, final String message, final Object... arguments) {
        if (obj1 != null && obj2 != null && !obj1.equals(obj2)) {
            throw new IllegalArgumentException(getMessage("Object1 is not equal to Object2.", message, arguments));
        } else if (obj1 == null && obj2 != null) {
            throw new IllegalArgumentException(getMessage("Object1 is null but not Object2.", message, arguments));
        } else if (obj1 != null && obj2 == null) {
            throw new IllegalArgumentException(getMessage("Object2 is null but not Object1.", message, arguments));
        }
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre class="code">
     * Assert.isEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and not the other one or
     *             are not equal.
     */
    public static void isEqual(final Object obj1, final Object obj2) {
        Assert.isEqual(obj1, obj2, null);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre class="code">
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param clazz
     *            the required class
     * @param obj
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public static void isInstanceOf(final Class<?> clazz, final Object obj) {
        isInstanceOf(clazz, obj, null);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre class="code">
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param obj
     *            the object to check
     * @param message
     *            the exception message, use the default assertion if null
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public static void isInstanceOf(final Class<?> type, final Object obj, final String message, final Object... arguments) {
        isNotNull(type, "Type to check against must not be null");
        if (!type.isInstance(obj)) {

            final String clazzName;
            if (obj != null) {
                clazzName = obj.getClass().getName();
            } else {
                clazzName = "null";
            }

            throw new IllegalArgumentException(
                    getMessage("Object of class [" + clazzName + "] must be an instance of " + type, message, arguments));
        }
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre class="code">
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param superType
     *            the super type to check
     * @param subType
     *            the sub type to check
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public static void isAssignable(final Class<?> superType, final Class<?> subType) {
        isAssignable(superType, subType, null);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre class="code">
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param superType
     *            the super type to check against
     * @param subType
     *            the sub type to check
     * @param message
     *            the exception message, use the default assertion if null
     * @param arguments
     *            the message arguments (use with String.format) message looks
     *            OK when appended to it.
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public static void isAssignable(final Class<?> superType, final Class<?> subType, final String message, final Object... arguments) {
        isNotNull(superType, "Type to check against must not be null");
        if (subType == null || !superType.isAssignableFrom(subType)) {
            throw new IllegalArgumentException(getMessage(subType + " is not assignable to " + superType, message, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalStateException} if
     * the test result is {@code false}. Call isTrue if you wish to throw
     * IllegalArgumentException on an assertion failure.
     * 
     * <pre class="code">
     * Assert.state(id == null, &quot;The id property must not already be initialized&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             if expression is {@code false}
     */
    public static void state(final boolean expression, final String message, final Object... arguments) {
        if (!expression) {
            throw new IllegalStateException(getMessage("this state invariant must be true", message, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@link IllegalStateException} if
     * the test result is {@code false}.
     * <p>
     * Call {@link #isTrue(boolean)} if you wish to throw
     * {@link IllegalArgumentException} on an assertion failure.
     * 
     * <pre class="code">
     * Assert.state(id == null);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalStateException
     *             if the supplied expression is {@code false}
     */
    public static void state(final boolean expression) {
        state(expression, null);
    }

    /**
     * Fail, throwing {@link IllegalStateException}.
     * 
     * <pre class="code">
     * Assert.fail(&quot;Error&quot;, new Exception());
     * </pre>
     * 
     * @param message
     *            a message, if {@code null} use the default assertion message
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             in all cases
     */
    public static void fail(final String message, final Object... arguments) {
        fail(null, message, arguments);
    }

    /**
     * Fail, throwing {@link IllegalStateException}.
     * 
     * <pre class="code">
     * Assert.fail(&quot;Error&quot;, new Exception());
     * </pre>
     * 
     * 
     * @param throwable
     *            the cause exception
     * @param message
     *            a message, if {@code null} use the default assertion message
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             in all cases
     */
    public static void fail(final Throwable throwable, final String message, final Object... arguments) {
        if (throwable != null) {
            throw new IllegalArgumentException(getMessage("", message, arguments), throwable);
        } else {
            throw new IllegalArgumentException(getMessage("", message, arguments));
        }
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param actual
     *            an object to check
     * @param matcher
     *            an hamcrest matcher
     * @param <T>
     *            the type of the object to check
     * @throws IllegalArgumentException
     *             if matcher doesn't match
     */
    public static <T> void that(final T actual, final Matcher<? super T> matcher) {
        Assert.that(actual, matcher, null);
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param actual
     *            an object to check
     * @param matcher
     *            an hamcrest matcher
     * @param message
     *            a message, if {@code null} use the default assertion message
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <T>
     *            the type of the object to check
     * @throws IllegalArgumentException
     *             if matcher doesn't match
     */
    public static <T> void that(final T actual, final Matcher<? super T> matcher, final String message, final Object... arguments) {
        if (!matcher.matches(actual)) {
            Description description = new StringDescription();
            description.appendText(getMessage("", message, arguments));
            description.appendText("\nExpected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(actual, description);

            throw new IllegalArgumentException(description.toString());
        }
    }

    private static String getMessage(final String defaultString, final String message, final Object... arguments) {
        final String msg;
        if (StringUtils.isNotEmpty(message)) {
            if (arguments != null && arguments.length > 0) {
                msg = String.format(Assert.locale, message, arguments);
            } else {
                msg = message;
            }
        } else {
            msg = defaultString;
        }
        return ASSERTION_FAILED + " " + msg;
    }
}
