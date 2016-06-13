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
package fr.landel.utils.asserts;

import fr.landel.utils.commons.StringUtils;

/**
 * Assertion utility class that assists in validating arguments for strings.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertCharSequence<S extends CharSequence> extends AssertObject<AssertCharSequence<S>, S> {

    protected AssertCharSequence(final S object) {
        super(object);
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(name);
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public AssertCharSequence<S> isNotEmpty() {
        return this.isNotEmpty((String) null);
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public AssertCharSequence<S> isNotEmpty(final String message, final Object... arguments) {
        AssertCharSequence.isNotEmpty(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that the given String is not empty; that is, it must not be
     * {@code null} and not the empty String.
     * 
     * <pre>
     * AssertUtils.isNotEmpty(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * 
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text is empty
     */
    public <E extends Throwable> AssertCharSequence<S> isNotEmpty(final E exception) throws E {
        AssertCharSequence.isNotEmpty(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isNotEmpty(final CharSequence text, final E exception, final String message,
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
     * @return this
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public AssertCharSequence<S> isEmpty() {
        return this.isEmpty((String) null);
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * AssertUtils.isEmpty(name, &quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the text is empty
     */
    public AssertCharSequence<S> isEmpty(final String message, final Object... arguments) {
        AssertCharSequence.isEmpty(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that the given String is {@code null} or empty.
     * 
     * <pre>
     * AssertUtils.isEmpty(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * 
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text is empty
     */
    public <E extends Throwable> AssertCharSequence<S> isEmpty(final E exception) throws E {
        AssertCharSequence.isEmpty(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isEmpty(final CharSequence text, final E exception, final String message,
            final Object... arguments) throws E {
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
     * @return this
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public AssertCharSequence<S> isNotBlank() {
        return this.isNotBlank((String) null);
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * AssertUtils.isNotBlank(name, &quot;'name' must not be empty&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public AssertCharSequence<S> isNotBlank(final String message, final Object... arguments) {
        AssertCharSequence.isNotBlank(this.get(), null, message, arguments);

        return this;
    }

    /**
     * Assert that the given String has valid text content; that is, it must not
     * be {@code null} and must contain at least one non-whitespace character.
     * 
     * <pre>
     * AssertUtils.isNotBlank(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * 
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text does not contain valid text content
     */
    public <E extends Throwable> AssertCharSequence<S> isNotBlank(final E exception) throws E {
        AssertCharSequence.isNotBlank(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isNotBlank(final CharSequence text, final E exception, final String message,
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
     * @return this
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public AssertCharSequence<S> isBlank() {
        return this.isBlank((String) null);
    }

    /**
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * AssertUtils.isBlank(name, &quot;'name' must be null, empty or blank&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the text does not contain valid text content
     */
    public AssertCharSequence<S> isBlank(final String message, final Object... arguments) {
        AssertCharSequence.isBlank(this.get(), null, message, arguments);

        return this;
    }

    /***
     * Assert that the given String is {@code null}, empty or has blank text
     * content.
     * 
     * <pre>
     * AssertUtils.isBlank(name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * 
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text does not contain valid text content
     */
    public <E extends Throwable> AssertCharSequence<S> isBlank(final E exception) throws E {
        AssertCharSequence.isBlank(this.get(), exception, null);

        return this;
    }

    private static <E extends Throwable> void isBlank(final CharSequence text, final E exception, final String message,
            final Object... arguments) throws E {
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
     * @param substring
     *            the substring to find within the text
     * @return this
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public AssertCharSequence<S> contains(final String substring) {
        return this.contains(substring, (String) null);
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
     * AssertUtils.contains(name, &quot;rod&quot;, &quot;Name must contain 'rod'&quot;);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public AssertCharSequence<S> contains(final String substring, final String message, final Object... arguments) {
        AssertCharSequence.contains(this.get(), substring, null, message, arguments);

        return this;
    }

    /**
     * Assert that the given text contains the given substring.
     * 
     * <pre>
     * AssertUtils.contains(name, &quot;rod&quot;, exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param exception
     *            the exception to throw on error
     * 
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the text contains the substring
     */
    public <E extends Throwable> AssertCharSequence<S> contains(final String substring, final E exception) throws E {
        AssertCharSequence.contains(this.get(), substring, exception, null);

        return this;
    }

    private static <E extends Throwable> void contains(final CharSequence textToSearch, final String substring, final E exception,
            final String message, final Object... arguments) throws E {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && !containsCharSequence(textToSearch, substring)) {
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
     * @param substring
     *            the substring to find within the text
     * @return this
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public AssertCharSequence<S> doesNotContain(final String substring) {
        return this.doesNotContain(substring, (String) null);
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
     * AssertUtils.doesNotContain(name, &quot;rod&quot;, &quot;Name must not contain 'rod'&quot;);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the text contains the substring
     */
    public AssertCharSequence<S> doesNotContain(final String substring, final String message, final Object... arguments) {
        AssertCharSequence.doesNotContain(this.get(), substring, null, message, arguments);

        return this;
    }

    /**
     * Assert that the given text does not contain the given substring.
     * 
     * <pre>
     * AssertUtils.doesNotContain(fullName, name, exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @return this
     * @throws E
     *             if the text contains the substring
     */
    public <E extends Throwable> AssertCharSequence<S> doesNotContain(final String substring, final E exception) throws E {
        AssertCharSequence.doesNotContain(this.get(), substring, exception, null);

        return this;
    }

    private static <E extends Throwable> void doesNotContain(final CharSequence textToSearch, final String substring, final E exception,
            final String message, final Object... arguments) throws E {
        if (StringUtils.isNotEmpty(textToSearch) && StringUtils.isNotEmpty(substring) && containsCharSequence(textToSearch, substring)) {
            manageExceptions("this String argument must not contain the substring [" + substring + "]", exception, message,
                    new Object[] {textToSearch, substring}, arguments);
        }
    }

    private static boolean containsCharSequence(final CharSequence textToSearch, final CharSequence substring) {
        int p = 0;
        int l = substring.length();
        for (int i = 0; i < textToSearch.length() & p < l; i++) {
            if (textToSearch.charAt(i) == substring.charAt(p)) {
                p++;
            }
        }
        return p == l;
    }
}