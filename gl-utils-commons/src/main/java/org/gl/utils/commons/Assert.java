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
import java.util.regex.Pattern;

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
 * <pre>
 * Assert.notNull(clazz, &quot;The class must not be null&quot;);
 * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
 * </pre>
 *
 * <p>
 * Mainly for internal use within the framework; consider
 * <a href="http://commons.apache.org/proper/commons-lang/">Apache's Commons
 * Lang</a> for a more comprehensive suite of {@code String} utilities.
 * 
 * <p>
 * To display parameters in exception messages, use %p or %1$p
 * </p>
 * 
 * <pre>
 * Assert.isGT(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
 * // Exception: "The number '10' is not greater than number '20'"
 * Assert.isGT(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
 * // Exception: "'20' &gt; '10'"
 * </pre>
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
    private static final Pattern PATTERN_PARAMETERS = Pattern.compile("(%(\\d+\\$)?p)");

    private static Locale locale = Locale.US;

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
     * <pre>
     * Assert.isFalse(i &gt; 0);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isFalse(final boolean expression) {
        isFalse(expression, (String) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * Assert.isFalse(i &gt; 0, &quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isFalse(final boolean expression, final String message, final Object... arguments) {
        if (expression) {
            throw new IllegalArgumentException(getMessage("this expression must be false", message, new Object[] {expression}, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * Assert.isFalse(i &gt; 0, exceptionToThrowOnError);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}
     */
    public static <E extends Throwable> void isFalse(final boolean expression, final E exception) throws E {
        if (expression) {
            exception.addSuppressed(new IllegalArgumentException("this expression must be false"));
            throw exception;
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assert.isTrue(i &gt; 0);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isTrue(final boolean expression) {
        isTrue(expression, (String) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isTrue(final boolean expression, final String message, final Object... arguments) {
        if (!expression) {
            throw new IllegalArgumentException(getMessage("this expression must be true", message, new Object[] {expression}, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assert.isTrue(i &gt; 0, exceptionToThrowOnError);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}
     */
    public static <E extends Throwable> void isTrue(final boolean expression, final E exception) throws E {
        if (!expression) {
            exception.addSuppressed(new IllegalArgumentException("this expression must be true"));
            throw exception;
        }
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre>
     * Assert.isNull(value);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public static void isNull(final Object object) {
        isNull(object, (String) null);
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre>
     * Assert.isNull(value, &quot;The value must be null&quot;);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public static void isNull(final Object object, final String message, final Object... arguments) {
        if (object != null) {
            throw new IllegalArgumentException(getMessage("the object argument must be null", message, new Object[] {object}, arguments));
        }
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre>
     * Assert.isNull(value, exceptionToThrowOnError);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object is not {@code null}
     */
    public static <E extends Throwable> void isNull(final Object object, final E exception) throws E {
        if (object != null) {
            exception.addSuppressed(new IllegalArgumentException("the object argument must be null"));
            throw exception;
        }
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre>
     * Assert.isNotNull(object);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public static void isNotNull(final Object object) {
        isNotNull(object, (String) null);
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre>
     * Assert.isNotNull(clazz, &quot;The class must not be null&quot;);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public static void isNotNull(final Object object, final String message, final Object... arguments) {
        if (object == null) {
            throw new IllegalArgumentException(
                    getMessage("this argument is required; it must not be null", message, new Object[] {object}, arguments));
        }
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre>
     * Assert.isNotNull(clazz, exceptionToThrowOnError);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object is {@code null}
     */
    public static <E extends Throwable> void isNotNull(final Object object, final E exception) throws E {
        if (object == null) {
            exception.addSuppressed(new IllegalArgumentException("this argument is required; it must not be null"));
            throw exception;
        }
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * Assert.isNotEmpty(name);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isNotEmpty(final String text) {
        isNotEmpty(text, (String) null);
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * Assert.isNotEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isNotEmpty(final String text, final String message, final Object... arguments) {
        if (StringUtils.isEmpty(text)) {
            throw new IllegalArgumentException(getMessage("this String argument must have length; it must not be null or empty", message,
                    new Object[] {text}, arguments));
        }
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * Assert.isNotEmpty(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text is empty
     */
    public static <E extends Throwable> void isNotEmpty(final String text, final E exception) throws E {
        if (StringUtils.isEmpty(text)) {
            exception.addSuppressed(new IllegalArgumentException("this String argument must have length; it must not be null or empty"));
            throw exception;
        }
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * Assert.isNotEmpty(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public static void isNotEmpty(Object[] array) {
        isNotEmpty(array, (String) null);
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * Assert.isNotEmpty(array, &quot;The array must have elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array is {@code null} or has no elements
     */
    public static void isNotEmpty(Object[] array, final String message, final Object... arguments) {
        if (ArrayUtils.isEmpty(array)) {
            throw new IllegalArgumentException(getMessage("this array must not be empty: it must contain at least 1 element", message,
                    new Object[] {array}, arguments));
        }
    }

    /**
     * Assert that an array has elements; that is, it must not be {@code null}
     * and must have at least one element.
     * 
     * <pre>
     * Assert.isNotEmpty(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object array is {@code null} or has no elements
     */
    public static <E extends Throwable> void isNotEmpty(Object[] array, final E exception) throws E {
        if (ArrayUtils.isEmpty(array)) {
            exception.addSuppressed(new IllegalArgumentException("this array must not be empty: it must contain at least 1 element"));
            throw exception;
        }
    }

    /**
     * Assert that a collection has elements; that is, it must not be
     * {@code null} and must have at least one element.
     * 
     * <pre>
     * Assert.isNotEmpty(collection, &quot;Collection must have elements&quot;);
     * </pre>
     * 
     * @param collection
     *            the collection to check
     * @throws IllegalArgumentException
     *             if the collection is {@code null} or has no elements
     */
    public static void isNotEmpty(final Collection<?> collection) {
        isNotEmpty(collection, (String) null);
    }

    /**
     * Assert that a collection has elements; that is, it must not be
     * {@code null} and must have at least one element.
     * 
     * <pre>
     * Assert.isNotEmpty(collection, &quot;Collection must have elements&quot;);
     * </pre>
     * 
     * @param collection
     *            the collection to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the collection is {@code null} or has no elements
     */
    public static void isNotEmpty(final Collection<?> collection, final String message, final Object... arguments) {
        if (CollectionUtils.isEmpty(collection)) {
            throw new IllegalArgumentException(getMessage("this collection must not be empty: it must contain at least 1 element", message,
                    new Object[] {collection}, arguments));
        }
    }

    /**
     * Assert that a collection has elements; that is, it must not be
     * {@code null} and must have at least one element.
     * 
     * <pre>
     * Assert.isNotEmpty(collection, exceptionToThrowOnError);
     * </pre>
     * 
     * @param collection
     *            the collection to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the collection is {@code null} or has no elements
     */
    public static <E extends Throwable> void isNotEmpty(final Collection<?> collection, final E exception) throws E {
        if (CollectionUtils.isEmpty(collection)) {
            exception.addSuppressed(new IllegalArgumentException("this collection must not be empty: it must contain at least 1 element"));
            throw exception;
        }
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * Assert.notEmpty(map);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public static void isNotEmpty(final Map<?, ?> map) {
        isNotEmpty(map, (String) null);
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * Assert.isNotEmpty(map, &quot;Map must have entries&quot;);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the map is {@code null} or has no entries
     */
    public static void isNotEmpty(final Map<?, ?> map, final String message, final Object... arguments) {
        if (MapUtils.isEmpty(map)) {
            throw new IllegalArgumentException(
                    getMessage("this map must not be empty; it must contain at least one entry", message, new Object[] {map}, arguments));
        }
    }

    /**
     * Assert that a Map has entries; that is, it must not be {@code null} and
     * must have at least one entry.
     * 
     * <pre>
     * Assert.isNotEmpty(map, exceptionToThrowOnError);
     * </pre>
     * 
     * @param map
     *            the map to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the map is {@code null} or has no entries
     */
    public static <E extends Throwable> void isNotEmpty(final Map<?, ?> map, final E exception) throws E {
        if (MapUtils.isEmpty(map)) {
            exception.addSuppressed(new IllegalArgumentException("this map must not be empty; it must contain at least one entry"));
            throw exception;
        }
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * Assert.isEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isEmpty(final String text) {
        isEmpty(text, (String) null);
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * Assert.isEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public static void isEmpty(final String text, final String message, final Object... arguments) {
        if (StringUtils.isNotEmpty(text)) {
            throw new IllegalArgumentException(
                    getMessage("this String argument must be null or empty", message, new Object[] {text}, arguments));
        }
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * Assert.isEmpty(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text is empty
     */
    public static <E extends Throwable> void isEmpty(final String text, final E exception) throws E {
        if (StringUtils.isNotEmpty(text)) {
            exception.addSuppressed(new IllegalArgumentException("this String argument must be null or empty"));
            throw exception;
        }
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * Assert.isNotBlank(name, &quot;'name' must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isNotBlank(final String text) {
        isNotBlank(text, (String) null);
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * Assert.isNotBlank(name, &quot;'name' must not be empty&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isNotBlank(final String text, final String message, final Object... arguments) {
        if (StringUtils.isBlank(text)) {
            throw new IllegalArgumentException(getMessage("this String argument must have text; it must not be null, empty, or blank",
                    message, new Object[] {text}, arguments));
        }
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * Assert.isNotBlank(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text does not contain valid text content
     */
    public static <E extends Throwable> void isNotBlank(final String text, final E exception) throws E {
        if (StringUtils.isBlank(text)) {
            exception.addSuppressed(
                    new IllegalArgumentException("this String argument must have text; it must not be null, empty, or blank"));
            throw exception;
        }
    }

    /**
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * Assert.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isBlank(final String text) {
        isBlank(text, (String) null);
    }

    /**
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * Assert.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public static void isBlank(final String text, final String message, final Object... arguments) {
        if (StringUtils.isNotBlank(text)) {
            throw new IllegalArgumentException(
                    getMessage("this String argument must be null, empty or blank", message, new Object[] {text}, arguments));
        }
    }

    /***
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * Assert.isBlank(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param text
     *            the String to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text does not contain valid text content
     */
    public static <E extends Throwable> void isBlank(final String text, final E exception) throws E {
        if (StringUtils.isNotBlank(text)) {
            exception.addSuppressed(new IllegalArgumentException("this String argument must be null, empty or blank"));
            throw exception;
        }
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
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
        contains(textToSearch, substring, (String) null);
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
     * Assert.contains(name, &quot;rod&quot;, &quot;Name must contain 'rod'&quot;);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public static void contains(final String textToSearch, final String substring, final String message, final Object... arguments) {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && !textToSearch.contains(substring)) {
            throw new IllegalArgumentException(getMessage("this String argument must contain the substring [" + substring + "]", message,
                    new Object[] {textToSearch, substring}, arguments));
        }
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
     * Assert.contains(name, &quot;rod&quot;, exceptionToThrowOnError);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text contains the substring
     */
    public static <E extends Throwable> void contains(final String textToSearch, final String substring, final E exception) throws E {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && !textToSearch.contains(substring)) {
            exception.addSuppressed(new IllegalArgumentException("this String argument must contain the substring [" + substring + "]"));
            throw exception;
        }
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
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
        doesNotContain(textToSearch, substring, (String) null);
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
     * Assert.doesNotContain(name, &quot;rod&quot;, &quot;Name must not contain 'rod'&quot;);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public static void doesNotContain(final String textToSearch, final String substring, final String message, final Object... arguments) {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && textToSearch.contains(substring)) {
            throw new IllegalArgumentException(getMessage("this String argument must not contain the substring [" + substring + "]",
                    message, new Object[] {textToSearch, substring}, arguments));
        }
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
     * Assert.doesNotContain(fullName, name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param textToSearch
     *            the text to search
     * @param substring
     *            the substring to find within the text
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text contains the substring
     */
    public static <E extends Throwable> void doesNotContain(final String textToSearch, final String substring, final E exception) throws E {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && textToSearch.contains(substring)) {
            exception
                    .addSuppressed(new IllegalArgumentException("this String argument must not contain the substring [" + substring + "]"));
            throw exception;
        }
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre>
     * Assert.hasNoNullElements(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void hasNoNullElements(Object[] array) {
        hasNoNullElements(array, (String) null);
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre>
     * Assert.hasNoNullElements(array, &quot;The array must have non-null elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void hasNoNullElements(Object[] array, final String message, final Object... arguments) {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    throw new IllegalArgumentException(
                            getMessage("this array must not contain any null elements", message, new Object[] {array}, arguments));
                }
            }
        }
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre>
     * Assert.hasNoNullElements(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if object array contains a {@code null} element. The standard
     *             exception is appended as suppressed.
     */
    public static <E extends Throwable> void hasNoNullElements(Object[] array, final E exception) throws E {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    exception.addSuppressed(new IllegalArgumentException("this array must not contain any null elements"));
                    throw exception;
                }
            }
        }
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre>
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
        Assert.isNotEqual(obj1, obj2, (String) null);
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre>
     * Assert.isNotEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public static void isNotEqual(final Object obj1, final Object obj2, final String message, final Object... arguments) {
        if (obj1 == null && obj2 == null) {
            throw new IllegalArgumentException(getMessage("Both objects are null.", message, new Object[] {obj1, obj2}, arguments));
        } else if (obj1 != null && obj2 != null && obj1.equals(obj2)) {
            throw new IllegalArgumentException(getMessage("Object1 is equal to Object2.", message, new Object[] {obj1, obj2}, arguments));
        }
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre>
     * Assert.isNotEqual(foo1, foo2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if both are {@code null} or if objects are not equal. The
     *             standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isNotEqual(final Object obj1, final Object obj2, final E exception) throws E {
        if (obj1 == null && obj2 == null) {
            exception.addSuppressed(new IllegalArgumentException("Both objects are null."));
            throw exception;
        } else if (obj1 != null && obj2 != null && obj1.equals(obj2)) {
            exception.addSuppressed(new IllegalArgumentException("Object1 is equal to Object2."));
            throw exception;
        }
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(foo1, foo2);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and if objects are not
     *             equal.
     */
    public static void isEqual(final Object obj1, final Object obj2) {
        Assert.isEqual(obj1, obj2, (String) null);
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and not the other one or
     *             are not equal.
     */
    public static void isEqual(final Object obj1, final Object obj2, final String message, final Object... arguments) {
        if (obj1 != null && obj2 != null && !obj1.equals(obj2)) {
            throw new IllegalArgumentException(
                    getMessage("Object1 is not equal to Object2.", message, new Object[] {obj1, obj2}, arguments));
        } else if (obj1 == null && obj2 != null) {
            throw new IllegalArgumentException(
                    getMessage("Object1 is null but not Object2.", message, new Object[] {obj1, obj2}, arguments));
        } else if (obj1 != null && obj2 == null) {
            throw new IllegalArgumentException(
                    getMessage("Object2 is null but not Object1.", message, new Object[] {obj1, obj2}, arguments));
        }
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(foo1, foo2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one object is {@code null} and if objects are not
     *             equal. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isEqual(final Object obj1, final Object obj2, final E exception) throws E {
        if (obj1 != null && obj2 != null && !obj1.equals(obj2)) {
            exception.addSuppressed(new IllegalArgumentException("Object1 is not equal to Object2."));
            throw exception;
        } else if (obj1 == null && obj2 != null) {
            exception.addSuppressed(new IllegalArgumentException("Object1 is null but not Object2."));
            throw exception;
        } else if (obj1 != null && obj2 == null) {
            exception.addSuppressed(new IllegalArgumentException("Object2 is null but not Object1."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(10, 20);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are not
     *             equal.
     */
    public static <N extends Number> void isEqual(final N number1, final N number2) {
        Assert.isEqual(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(10, 20, &quot;The numbers are not equal&quot;);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number are not
     *             equal.
     */
    public static <N extends Number & Comparable<N>> void isEqual(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) == 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number message the exception message, use the
     *            default assertion if null
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number are not
     *             equal. The standard exception is appended as suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isEqual(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) == 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not equal to number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assert.isGT(10, 20);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             If at least one number is {@code null} and if number1 is not
     *             greater than number2.
     */
    public static <N extends Number & Comparable<N>> void isGT(final N number1, final N number2) {
        Assert.isGT(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is greater than the second one. In message,
     * parameters can be used through format '%p' (first %p will be replaced by
     * first parameter, second...).
     * 
     * 
     * <pre>
     * Assert.isGT(10, 20, &quot;The number1 is not greater than number2&quot;);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             If at least one number is {@code null} and if number is not
     *             greater than number2
     */
    public static <N extends Number & Comparable<N>> void isGT(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) <= 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not greater than number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is greater than the second one.
     * 
     * <pre>
     * Assert.isGT(10, 20, exceptionToThrowOnError);
     * </pre>
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             If at least one number is {@code null} and if number is not
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isGT(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) <= 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not greater than number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isGTE(final N number1, final N number2) {
        Assert.isGTE(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isGTE(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) < 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not greater than or equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is greater than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             lower than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isGTE(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) < 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not greater than or equal to number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isLT(final N number1, final N number2) {
        Assert.isLT(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2.
     */
    public static <N extends Number & Comparable<N>> void isLT(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) >= 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not lower than number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is lower than the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is not
     *             lower than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isLT(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) >= 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not not lower than number2."));
            throw exception;
        }
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2.
     */
    public static <N extends Number & Comparable<N>> void isLTE(final N number1, final N number2) {
        Assert.isLTE(number1, number2, (String) null);
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @throws IllegalArgumentException
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2.
     */
    public static <N extends Number & Comparable<N>> void isLTE(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (compareNumber(number1, number2, message, arguments) > 0) {
            throw new IllegalArgumentException(
                    getMessage("number1 is not lower than or equal to number2.", message, new Object[] {number1, number2}, arguments));
        }
    }

    /**
     * Assert that the first number is lower than or equal to the second one.
     * 
     * @param number1
     *            The first number
     * @param number2
     *            The second number
     * @param exception
     *            the exception to throw on error
     * @param <N>
     *            The type of each number <code>Byte</code>, <code>Short</code>,
     *            <code>Integer</code>, <code>Long</code>, <code>Float</code>,
     *            <code>Double</code>, <code>BigInteger</code> or
     *            <code>BigDecimal</code>. Supports new <code>Number</code>,
     *            types only if it implements <code>Comparable</code>.
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one number is {@code null} and if number1 is
     *             greater than number2. The standard exception is appended as
     *             suppressed.
     */
    public static <N extends Number & Comparable<N>, E extends Throwable> void isLTE(final N number1, final N number2, final E exception)
            throws E {
        if (compareNumber(number1, number2, exception) > 0) {
            exception.addSuppressed(new IllegalArgumentException("number1 is not lower than or equal to number2."));
            throw exception;
        }
    }

    private static <N extends Number & Comparable<N>> int compareNumber(final N number1, final N number2, final String message,
            final Object... arguments) {
        if (number1 != null && number2 != null) {
            return number1.compareTo(number2);
        } else if (number1 != null) {
            throw new IllegalArgumentException(getMessage("number1 is null.", message, new Object[] {number1, number2}, arguments));
        } else if (number2 != null) {
            throw new IllegalArgumentException(getMessage("number2 is null.", message, new Object[] {number1, number2}, arguments));
        } else {
            throw new IllegalArgumentException(
                    getMessage("number1 and number2 are null.", message, new Object[] {number1, number2}, arguments));
        }
    }

    private static <N extends Number & Comparable<N>, E extends Throwable> int compareNumber(final N number1, final N number2,
            final E exception) throws E {
        if (number1 != null && number2 != null) {
            return number1.compareTo(number2);
        } else if (number1 != null) {
            exception.addSuppressed(new IllegalArgumentException("number1 is null."));
        } else if (number2 != null) {
            exception.addSuppressed(new IllegalArgumentException("number2 is null."));
        } else {
            exception.addSuppressed(new IllegalArgumentException("number1 and number2 are null."));
        }
        throw exception;
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
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
        isInstanceOf(clazz, obj, (String) null);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param obj
     *            the object to check
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public static void isInstanceOf(final Class<?> type, final Object obj, final String message, final Object... arguments) {
        isNotNull(type, "Type to check against must not be null");
        if (!type.isInstance(obj)) {

            final String clazzName = getClassName(obj);

            throw new IllegalArgumentException(getMessage("Object of class [" + clazzName + "] must be an instance of " + type, message,
                    new Object[] {type, obj}, arguments));
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param obj
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if condition doesn't match.
     * @see Class#isInstance
     */
    public static <E extends Throwable> void isInstanceOf(final Class<?> type, final Object obj, final E exception) throws E {
        isNotNull(type, "Type to check against must not be null", exception);
        if (!type.isInstance(obj)) {

            final String clazzName = getClassName(obj);

            exception.addSuppressed(new IllegalArgumentException("Object of class [" + clazzName + "] must be an instance of " + type));
            throw exception;
        }
    }

    private static String getClassName(final Object obj) {
        final String clazzName;
        if (obj != null) {
            clazzName = obj.getClass().getName();
        } else {
            clazzName = "null";
        }
        return clazzName;
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
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
        isAssignable(superType, subType, (String) null);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param superType
     *            the super type to check against
     * @param subType
     *            the sub type to check
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format) message looks
     *            OK when appended to it.
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public static void isAssignable(final Class<?> superType, final Class<?> subType, final String message, final Object... arguments) {
        isNotNull(superType, "Type to check against must not be null");
        if (subType == null || !superType.isAssignableFrom(subType)) {
            throw new IllegalArgumentException(
                    getMessage(subType + " is not assignable to " + superType, message, new Object[] {superType, subType}, arguments));
        }
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param superType
     *            the super type to check against
     * @param subType
     *            the sub type to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the classes are not assignable. The standard exception is
     *             appended as suppressed.
     */
    public static <E extends Throwable> void isAssignable(final Class<?> superType, final Class<?> subType, final E exception) throws E {
        isNotNull(superType, "Type to check against must not be null");
        if (subType == null || !superType.isAssignableFrom(subType)) {
            exception.addSuppressed(new IllegalArgumentException(subType + " is not assignable to " + superType));
            throw exception;
        }
    }

    /**
     * Assert a boolean expression, throwing {@link IllegalStateException} if
     * the test result is {@code false}.
     * <p>
     * Call {@link #isTrue(boolean)} if you wish to throw
     * {@link IllegalArgumentException} on an assertion failure.
     * 
     * <pre>
     * Assert.state(id == null);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalStateException
     *             if the supplied expression is {@code false}
     */
    public static void state(final boolean expression) {
        state(expression, (String) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalStateException} if
     * the test result is {@code false}. Call isTrue if you wish to throw
     * IllegalArgumentException on an assertion failure.
     * 
     * <pre>
     * Assert.state(id == null, &quot;The id property must not already be initialized&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             if expression is {@code false}
     */
    public static void state(final boolean expression, final String message, final Object... arguments) {
        if (!expression) {
            throw new IllegalStateException(getMessage("this state invariant must be true", message, new Object[] {expression}, arguments));
        }
    }

    /**
     * Assert a boolean expression, appending {@code IllegalStateException} if
     * the test result is {@code false}. Call isTrue if you wish to throw
     * IllegalArgumentException on an assertion failure.
     * 
     * <pre>
     * Assert.state(id == null, &quot;The id property must not already be initialized&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}. The standard exception is
     *             appended as suppressed.
     */
    public static <E extends Throwable> void state(final boolean expression, final E exception) throws E {
        if (!expression) {
            exception.addSuppressed(new IllegalStateException("this state invariant must be true"));
            throw exception;
        }
    }

    /**
     * Fail, throwing {@link IllegalStateException}.
     * 
     * <pre>
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
     * <pre>
     * Assert.fail(&quot;Error&quot;, new Exception());
     * </pre>
     * 
     * 
     * @param throwable
     *            the cause exception
     * @param message
     *            a message, if {@code null} use the default assertion message
     *            (%p or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             in all cases
     */
    public static void fail(final Throwable throwable, final String message, final Object... arguments) {
        if (throwable != null) {
            throw new IllegalArgumentException(getMessage("", message, new Object[] {throwable}, arguments), throwable);
        } else {
            throw new IllegalArgumentException(getMessage("", message, new Object[] {throwable}, arguments));
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
        Assert.that(actual, matcher, (String) null);
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
     *            (%p or %1$p can be used to display parameter value, see
     *            explanation in the class description)
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
            description.appendText(getMessage("", message, new Object[] {actual, matcher}, arguments));
            description.appendText("\nExpected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(actual, description);

            throw new IllegalArgumentException(description.toString());
        }
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param actual
     *            an object to check
     * @param matcher
     *            an hamcrest matcher
     * @param exception
     *            the exception to throw on error
     * @param <T>
     *            the type of the object to check
     * @param <E>
     *            The type of exception
     * @throws E
     *             if condition doesn't match.
     */
    public static <T, E extends Throwable> void that(final T actual, final Matcher<? super T> matcher, final E exception) throws E {
        if (!matcher.matches(actual)) {
            Description description = new StringDescription();
            description.appendText("Expected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(actual, description);

            exception.addSuppressed(new IllegalArgumentException(description.toString()));
            throw exception;
        }
    }

    private static String getMessage(final String defaultString, final String message, final Object[] parameters,
            final Object[] arguments) {
        String msg;
        String group;
        String replacement = null;
        int number;

        if (StringUtils.isNotEmpty(message)) {
            msg = message;
            if (parameters != null && parameters.length > 0) {
                java.util.regex.Matcher matcher;
                int count = 0;
                while ((matcher = PATTERN_PARAMETERS.matcher(msg)).find()) {
                    group = matcher.group(0);

                    if (group.indexOf('$') > -1) {
                        number = NumberUtils.parseInt(StringUtils.remove(StringUtils.remove(group, '%'), "$p"), 0);
                        if (number > 0 && number <= parameters.length) {
                            replacement = String.valueOf(parameters[number - 1]);
                        } else {
                            replacement = "";
                        }
                    } else if (count < parameters.length) {
                        replacement = String.valueOf(parameters[count]);
                        count++;
                    } else {
                        replacement = "";
                    }
                    if (replacement != null) {
                        msg = StringUtils.replace(msg, replacement, matcher.start(), matcher.end());
                        replacement = null;
                    }
                }
            }
            if (arguments != null && arguments.length > 0) {
                msg = String.format(Assert.locale, msg, arguments);
            }
        } else {
            msg = defaultString;
        }
        return ASSERTION_FAILED + " " + msg;
    }
}