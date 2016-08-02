/*
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.util.Locale;
import java.util.regex.Pattern;

import fr.landel.utils.commons.StringUtils;

/**
 * Assertion utility class that assists in validating arguments for strings.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class AssertCharSequence<S extends CharSequence> extends AssertObject<AssertCharSequence<S>, S> {

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     */
    protected AssertCharSequence(final S object) {
        super(object, TYPE.CHAR_SEQUENCE);
    }

    /**
     * Asserts that the given String has the specified length. The input cannot
     * not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <pre>
     * Assertor.that(name).hasLength(5).toThrow();
     * </pre>
     * 
     * @param length
     *            The length (cannot be lower than 0)
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> hasLength(final int length) {
        return this.hasLength(length, this.msg(MSG.CSQ.LENGTH, this.getParam(), this.getNextParam(1, TYPE.NUMBER_INTEGER)));
    }

    /**
     * Asserts that the given String has the specified length. The input cannot
     * not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <pre>
     * Assertor.that(name).hasLength(5, "Name has not the expected length").toThrow();
     * </pre>
     * 
     * @param length
     *            The length (cannot be lower than 0)
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> hasLength(final int length, final CharSequence message, final Object... arguments) {
        return this.hasLength(length, null, message, arguments);
    }

    /**
     * Asserts that the given String has the specified length. The input cannot
     * not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <pre>
     * Assertor.that(name).hasLength(5, Locale.US, "The field '%s' has not the expected length %1$s*", "name").toThrow();
     * // on mismatch, the exception message =&gt; The field 'name' has not the
     * // expected length 5
     * </pre>
     * 
     * @param length
     *            The length (cannot be lower than 0)
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> hasLength(final int length, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(length >= 0 && this.get() != null, () -> this.get().length() == length, () -> this.msg(MSG.CSQ.LENGTH, true),
                message, arguments, locale, length);
    }

    /**
     * Asserts that the given {@code String} is not empty; that is, it must not
     * be {@code null} and not the empty {@code String}.
     * 
     * <pre>
     * Assertor.that(name).isNotEmpty().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotEmpty() {
        return this.isNotEmpty(this.msg(MSG.CSQ.EMPTY + MSG.NOT));
    }

    /**
     * Asserts that the given {@code String} is not empty; that is, it must not
     * be {@code null} and not the empty {@code String}.
     * 
     * <pre>
     * Assertor.that(name).isNotEmpty("Name cannot be filled").toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    /**
     * Asserts that the given {@code String} is not empty; that is, it must not
     * be {@code null} and not the empty {@code String}.
     * 
     * <pre>
     * Assertor.that(name).isNotEmpty(Locale.US, "Param '%1$s*' cannot be filled (%.2fms)", "name", 1.563).toThrow();
     * // Message of exception if name is not empty and not null:
     * // =&gt; "Param 'name' cannot be filled (1.56ms)"
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> StringUtils.isNotEmpty(this.get()), null, message, arguments, locale);
    }

    /**
     * Asserts that the given {@code String} is {@code null} or empty.
     * 
     * <pre>
     * Assertor.that(name).isEmpty().toThrow(&quot;Name must not be empty&quot;);
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isEmpty() {
        return this.isEmpty(this.msg(MSG.CSQ.EMPTY, this.getParam()));
    }

    /**
     * Asserts that the given {@code String} is {@code null} or empty.
     * 
     * <pre>
     * Assertor.that(name).isEmpty("Name must not be empty").toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    /**
     * Asserts that the given {@code String} is {@code null} or empty.
     * 
     * <pre>
     * Assertor.that(name).isEmpty(Locale.US, "Param '%1$s*' must not be empty (%.2fms)", "name", 25.236f).toThrow();
     * // Message of exception if name is null or empty:
     * // =&gt; "Param 'name' must not be empty (25.24ms)"
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> StringUtils.isEmpty(this.get()), null, message, arguments, locale);
    }

    /**
     * Asserts that the given {@code String} has valid text content; that is, it
     * must not be {@code null} and must contain at least one non-whitespace
     * character.
     * 
     * <pre>
     * Assertor.that(name).isNotBlank().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotBlank() {
        return this.isNotBlank(this.msg(MSG.CSQ.BLANK + MSG.NOT));
    }

    /**
     * Asserts that the given {@code String} has valid text content; that is, it
     * must not be {@code null} and must contain at least one non-whitespace
     * character.
     * 
     * <pre>
     * Assertor.that(name).isNotBlank("Cannot be blank").toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotBlank(final CharSequence message, final Object... arguments) {
        return this.isNotBlank(null, message, arguments);
    }

    /**
     * Asserts that the given {@code String} has valid text content; that is, it
     * must not be {@code null} and must contain at least one non-whitespace
     * character.
     * 
     * <pre>
     * Assertor.that(name).isNotBlank().toThrow();
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isNotBlank(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> StringUtils.isNotBlank(this.get()), null, message, arguments, locale);
    }

    /**
     * Asserts that the given {@code String} is {@code null}, empty or has blank
     * text content.
     * 
     * <pre>
     * Assertor.that(name).isBlank().toThrow();
     * </pre>
     * 
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isBlank() {
        return this.isBlank(this.msg(MSG.CSQ.BLANK, this.getParam()));
    }

    /**
     * Asserts that the given {@code String} is {@code null}, empty or has blank
     * text content.
     * 
     * <pre>
     * Assertor.that(name).isBlank().toThrow();
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isBlank(final CharSequence message, final Object... arguments) {
        return this.isBlank(null, message, arguments);
    }

    /**
     * Asserts that the given {@code String} is {@code null}, empty or has blank
     * text content.
     * 
     * <pre>
     * Assertor.that(name).isBlank().toThrow();
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> isBlank(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> StringUtils.isBlank(this.get()), null, message, arguments, locale);
    }

    /**
     * Asserts that the given text contains the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).contains(name).toThrow();
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> contains(final CharSequence substring) {
        return this.contains(substring, this.msg(MSG.CSQ.CONTAINS, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the given text contains the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).contains(name).toThrow();
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> contains(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.contains(substring, null, message, arguments);
    }

    /**
     * Asserts that the given text contains the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).contains(name).toThrow();
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> contains(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && StringUtils.isNotEmpty(substring), () -> containsCharSequence(this.get(), substring),
                () -> this.msg(MSG.CSQ.CONTAINS, true), message, arguments, locale, substring);
    }

    /**
     * Asserts that the given text starts with the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).startsWith(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> startsWith(final CharSequence substring) {
        return this.startsWith(substring, this.msg(MSG.CSQ.STARTS, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the given text starts with the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).startsWith(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> startsWith(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.startsWith(substring, null, message, arguments);
    }

    /**
     * Asserts that the given text starts with the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).startsWith(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> startsWith(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && StringUtils.isNotEmpty(substring), () -> StringUtils.startsWith(this.get(), substring),
                () -> this.msg(MSG.CSQ.STARTS, true), message, arguments, locale, substring);
    }

    /**
     * Asserts that the given text starts with the given substring (insensitive
     * case).
     * 
     * <pre>
     * Assertor.that(fullName).startsWithIgnoreCase(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> startsWithIgnoreCase(final CharSequence substring) {
        return this.startsWithIgnoreCase(substring, this.msg(MSG.CSQ.STARTS, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the given text starts with the given substring (insensitive
     * case).
     * 
     * <pre>
     * Assertor.that(fullName).startsWithIgnoreCase(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> startsWithIgnoreCase(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.startsWithIgnoreCase(substring, null, message, arguments);
    }

    /**
     * Asserts that the given text starts with the given substring (insensitive
     * case).
     * 
     * <pre>
     * Assertor.that(fullName).startsWithIgnoreCase(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> startsWithIgnoreCase(final CharSequence substring, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return this.combine(this.get() != null && StringUtils.isNotEmpty(substring),
                () -> StringUtils.startsWithIgnoreCase(this.get(), substring), () -> this.msg(MSG.CSQ.STARTS, true), message, arguments,
                locale, substring);
    }

    /**
     * Asserts that the given text ends with the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).endsWith(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> endsWith(final CharSequence substring) {
        return this.endsWith(substring, this.msg(MSG.CSQ.ENDS, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the given text ends with the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).endsWith(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> endsWith(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.endsWith(substring, null, message, arguments);
    }

    /**
     * Asserts that the given text ends with the given substring.
     * 
     * <pre>
     * Assertor.that(fullName).endsWith(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> endsWith(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && StringUtils.isNotEmpty(substring), () -> StringUtils.endsWith(this.get(), substring),
                () -> this.msg(MSG.CSQ.ENDS, true), message, arguments, locale, substring);
    }

    /**
     * Asserts that the given text ends with the given substring (insensitive
     * case).
     * 
     * <pre>
     * Assertor.that(fullName).endsWithIgnoreCase(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> endsWithIgnoreCase(final CharSequence substring) {
        return this.endsWithIgnoreCase(substring, this.msg(MSG.CSQ.ENDS, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the given text ends with the given substring (insensitive
     * case).
     * 
     * <pre>
     * Assertor.that(fullName).endsWithIgnoreCase(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> endsWithIgnoreCase(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.endsWithIgnoreCase(substring, null, message, arguments);
    }

    /**
     * Asserts that the given text ends with the given substring (insensitive
     * case).
     * 
     * <pre>
     * Assertor.that(fullName).endsWithIgnoreCase(name).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param substring
     *            the substring to find within the text
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> endsWithIgnoreCase(final CharSequence substring, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return this.combine(this.get() != null && StringUtils.isNotEmpty(substring),
                () -> StringUtils.endsWithIgnoreCase(this.get(), substring), () -> this.msg(MSG.CSQ.ENDS, true), message, arguments, locale,
                substring);
    }

    /**
     * Asserts that the given text matches the pattern.
     * 
     * <pre>
     * Assertor.that(fullName).matches(pattern).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param pattern
     *            the pattern
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> matches(final Pattern pattern) {
        return this.matches(pattern, this.msg(MSG.CSQ.MATCHES, this.getParam(), this.getNextParam(1, TYPE.UNKNOWN)));
    }

    /**
     * Asserts that the given text matches the pattern.
     * 
     * <pre>
     * Assertor.that(fullName).matches(pattern).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param pattern
     *            the pattern
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> matches(final Pattern pattern, final CharSequence message, final Object... arguments) {
        return this.matches(pattern, null, message, arguments);
    }

    /**
     * Asserts that the given text matches the pattern.
     * 
     * <pre>
     * Assertor.that(fullName).matches(pattern).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param pattern
     *            the pattern
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> matches(final Pattern pattern, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && pattern != null, () -> pattern.matcher(this.get()).matches(),
                () -> this.msg(MSG.CSQ.MATCHES, true), message, arguments, locale, pattern);
    }

    /**
     * Asserts that the given text matches the regular expression.
     * 
     * <pre>
     * Assertor.that(fullName).matches(regex).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param regex
     *            the regular expression
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> matches(final CharSequence regex) {
        return this.matches(regex, this.msg(MSG.CSQ.MATCHES, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the given text matches the regular expression.
     * 
     * <pre>
     * Assertor.that(fullName).matches(regex).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param regex
     *            the regular expression
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> matches(final CharSequence regex, final CharSequence message, final Object... arguments) {
        return this.matches(regex, null, message, arguments);
    }

    /**
     * Asserts that the given text matches the regular expression.
     * 
     * <pre>
     * Assertor.that(fullName).matches(regex).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param regex
     *            the regular expression
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> matches(final CharSequence regex, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && regex != null, () -> Pattern.matches(regex.toString(), this.get()),
                () -> this.msg(MSG.CSQ.MATCHES, true), message, arguments, locale, regex);
    }

    /**
     * Asserts that the pattern can be found in the give text.
     * 
     * <pre>
     * Assertor.that(fullName).find(pattern).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param pattern
     *            the pattern
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> find(final Pattern pattern) {
        return this.find(pattern, this.msg(MSG.CSQ.FIND, this.getParam(), this.getNextParam(1, TYPE.UNKNOWN)));
    }

    /**
     * Asserts that the pattern can be found in the give text.
     * 
     * <pre>
     * Assertor.that(fullName).find(pattern).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param pattern
     *            the pattern
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> find(final Pattern pattern, final CharSequence message, final Object... arguments) {
        return this.find(pattern, null, message, arguments);
    }

    /**
     * Asserts that the pattern can be found in the give text.
     * 
     * <pre>
     * Assertor.that(fullName).find(pattern).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param pattern
     *            the pattern
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> find(final Pattern pattern, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && pattern != null, () -> pattern.matcher(this.get()).find(),
                () -> this.msg(MSG.CSQ.FIND, true), message, arguments, locale, pattern);
    }

    /**
     * Asserts that the regular expression can be found in the give text.
     * 
     * <pre>
     * Assertor.that(fullName).find(regex).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param regex
     *            the regular expression
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> find(final CharSequence regex) {
        return this.find(regex, this.msg(MSG.CSQ.FIND, this.getParam(), this.getNextParam(1, TYPE.CHAR_SEQUENCE)));
    }

    /**
     * Asserts that the regular expression can be found in the give text.
     * 
     * <pre>
     * Assertor.that(fullName).find(regex).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param regex
     *            the regular expression
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> find(final CharSequence regex, final CharSequence message, final Object... arguments) {
        return this.find(regex, null, message, arguments);
    }

    /**
     * Asserts that the regular expression can be found in the give text.
     * 
     * <pre>
     * Assertor.that(fullName).find(regex).toThrow(exceptionToThrowOnError);
     * </pre>
     * 
     * @param regex
     *            the regular expression
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<AssertCharSequence<S>, S> find(final CharSequence regex, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && regex != null, () -> Pattern.compile(regex.toString()).matcher(this.get()).find(),
                () -> this.msg(MSG.CSQ.FIND, true), message, arguments, locale, regex);
    }

    /**
     * Searches in char sequence, if the specified sub sequence exists in.
     * {@code null} values have to be checked before.
     * 
     * @param textToSearch
     *            where to search
     * @param substring
     *            chat to search
     * @return {@code true} if found, {@code false} otherwise
     */
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
