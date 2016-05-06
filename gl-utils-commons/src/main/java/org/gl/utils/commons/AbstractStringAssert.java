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
import java.util.Map;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ArrayUtils;

/**
 * Abstract part see Assert class.
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
public abstract class AbstractStringAssert extends AbstractAssert {

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
}