/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.commons.asserts;

import fr.landel.utils.commons.StringUtils;

/**
 * Assertion utility class that assists in validating arguments for strings.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public abstract class AbstractStringAssert extends AbstractAssert {

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(name);
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
     * AssertUtils.isNotEmpty(name, &quot;Name must not be empty&quot;);
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
        isNotEmpty(text, null, message, arguments);
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(name, exceptionToThrowOnError);
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
        isNotEmpty(text, exception, null);
    }

    private static <E extends Throwable> void isNotEmpty(final String text, final E exception, final String message,
            final Object... arguments) throws E {
        if (StringUtils.isEmpty(text)) {
            manageExceptions("this String argument must have length; it must not be null or empty", exception, message, new Object[] {text},
                    arguments);
        }
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * AssertUtils.isEmpty(name, &quot;Name must not be empty&quot;);
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
     * AssertUtils.isEmpty(name, &quot;Name must not be empty&quot;);
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
        isEmpty(text, null, message, arguments);
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * AssertUtils.isEmpty(name, exceptionToThrowOnError);
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
        isEmpty(text, exception, null);
    }

    private static <E extends Throwable> void isEmpty(final String text, final E exception, final String message, final Object... arguments)
            throws E {
        if (StringUtils.isNotEmpty(text)) {
            manageExceptions("this String argument must be null or empty", exception, message, new Object[] {text}, arguments);
        }
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * AssertUtils.isNotBlank(name, &quot;'name' must not be empty&quot;);
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
     * AssertUtils.isNotBlank(name, &quot;'name' must not be empty&quot;);
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
        isNotBlank(text, null, message, arguments);
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * AssertUtils.isNotBlank(name, exceptionToThrowOnError);
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
        isNotBlank(text, exception, null);
    }

    private static <E extends Throwable> void isNotBlank(final String text, final E exception, final String message,
            final Object... arguments) throws E {
        if (StringUtils.isBlank(text)) {
            manageExceptions("this String argument must have text; it must not be null, empty, or blank", exception, message,
                    new Object[] {text}, arguments);
        }
    }

    /**
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * AssertUtils.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
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
     * AssertUtils.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
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
        isBlank(text, null, message, arguments);
    }

    /***
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * AssertUtils.isBlank(name, exceptionToThrowOnError);
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
        isBlank(text, exception, null);
    }

    private static <E extends Throwable> void isBlank(final String text, final E exception, final String message, final Object... arguments)
            throws E {
        if (StringUtils.isNotBlank(text)) {
            manageExceptions("this String argument must be null, empty or blank", exception, message, new Object[] {text}, arguments);
        }
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
     * AssertUtils.contains(name, &quot;rod&quot;);
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
     * AssertUtils.contains(name, &quot;rod&quot;, &quot;Name must contain 'rod'&quot;);
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
        contains(textToSearch, substring, null, message, arguments);
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
     * AssertUtils.contains(name, &quot;rod&quot;, exceptionToThrowOnError);
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
        contains(textToSearch, substring, exception, null);
    }

    private static <E extends Throwable> void contains(final String textToSearch, final String substring, final E exception,
            final String message, final Object... arguments) throws E {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && !textToSearch.contains(substring)) {
            manageExceptions("this String argument must contain the substring [" + substring + "]", exception, message,
                    new Object[] {textToSearch, substring}, arguments);
        }
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
     * AssertUtils.doesNotContain(name, &quot;rod&quot;);
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
     * AssertUtils.doesNotContain(name, &quot;rod&quot;, &quot;Name must not contain 'rod'&quot;);
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
        doesNotContain(textToSearch, substring, null, message, arguments);
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
     * AssertUtils.doesNotContain(fullName, name, exceptionToThrowOnError);
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
        doesNotContain(textToSearch, substring, exception, null);
    }

    private static <E extends Throwable> void doesNotContain(final String textToSearch, final String substring, final E exception,
            final String message, final Object... arguments) throws E {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && textToSearch.contains(substring)) {
            manageExceptions("this String argument must not contain the substring [" + substring + "]", exception, message,
                    new Object[] {textToSearch, substring}, arguments);
        }
    }
}
