/*-
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

/**
 * This class define methods that can be applied on the checked char sequence.
 * Each method return a {@link PredicateStepCharSequence}
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 * @param <T>
 *            the type of assertor result
 */
@FunctionalInterface
public interface PredicateAssertorCharSequence<T extends CharSequence> extends PredicateAssertor<PredicateStepCharSequence<T>, T> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepCharSequence<T> get(final AssertorResult<T> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorCharSequence<T> not() {
        return () -> HelperAssertor.not(getResult());
    }

    /**
     * Asserts that the given char sequence has the specified length. The input
     * cannot not be {@code null} and the length cannot be lower than 0 (returns
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
    default PredicateStepCharSequence<T> hasLength(final int length) {
        return this.hasLength(length, null);
    }

    /**
     * Asserts that the given char sequence has the specified length. The input
     * cannot not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <pre>
     * Assertor.that(name).hasLength(5, "not the good length").toThrow();
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
    default PredicateStepCharSequence<T> hasLength(final int length, final CharSequence message, final Object... arguments) {
        return this.hasLength(length, null, message, arguments);
    }

    /**
     * Asserts that the given char sequence has the specified length. The input
     * cannot not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <pre>
     * Assertor.that(name).hasLength(5, Locale.US, "not the good length").toThrow();
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
    default PredicateStepCharSequence<T> hasLength(final int length, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.hasLength(this.getResult(), length, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> isEmpty() {
        return this.isEmpty(null);
    }

    default PredicateStepCharSequence<T> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isEmpty(this.getResult(), locale, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEmpty() {
        return this.isNotEmpty(null);
    }

    default PredicateStepCharSequence<T> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isNotEmpty(this.getResult(), locale, message, arguments);
    }

    default PredicateStepCharSequence<T> isBlank() {
        return this.isBlank(null);
    }

    default PredicateStepCharSequence<T> isBlank(final CharSequence message, final Object... arguments) {
        return this.isBlank(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isBlank(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isBlank(this.getResult(), locale, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotBlank() {
        return this.isNotBlank(null);
    }

    default PredicateStepCharSequence<T> isNotBlank(final CharSequence message, final Object... arguments) {
        return this.isNotBlank(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotBlank(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isNotBlank(this.getResult(), locale, message, arguments);
    }

    default PredicateStepCharSequence<T> contains(final CharSequence substring) {
        return this.contains(substring, null);
    }

    default PredicateStepCharSequence<T> contains(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.contains(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> contains(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.contains(this.getResult(), substring, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> startsWith(final CharSequence substring) {
        return this.startsWith(substring, null);
    }

    default PredicateStepCharSequence<T> startsWith(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.startsWith(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> startsWith(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.startsWith(this.getResult(), substring, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> startsWithIgnoreCase(final CharSequence substring) {
        return this.startsWithIgnoreCase(substring, null);
    }

    default PredicateStepCharSequence<T> startsWithIgnoreCase(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.startsWithIgnoreCase(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> startsWithIgnoreCase(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.startsWithIgnoreCase(this.getResult(), substring, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> endsWith(final CharSequence substring) {
        return this.endsWith(substring, null);
    }

    default PredicateStepCharSequence<T> endsWith(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.endsWith(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> endsWith(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.endsWith(this.getResult(), substring, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> endsWithIgnoreCase(final CharSequence substring) {
        return this.endsWithIgnoreCase(substring, null);
    }

    default PredicateStepCharSequence<T> endsWithIgnoreCase(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.endsWithIgnoreCase(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> endsWithIgnoreCase(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.endsWithIgnoreCase(this.getResult(), substring, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> matches(final Pattern pattern) {
        return this.matches(pattern, null);
    }

    default PredicateStepCharSequence<T> matches(final Pattern pattern, final CharSequence message, final Object... arguments) {
        return this.matches(pattern, null, message, arguments);
    }

    default PredicateStepCharSequence<T> matches(final Pattern pattern, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.matches(this.getResult(), pattern, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> matches(final CharSequence regex) {
        return this.matches(regex, null);
    }

    default PredicateStepCharSequence<T> matches(final CharSequence regex, final CharSequence message, final Object... arguments) {
        return this.matches(regex, null, message, arguments);
    }

    default PredicateStepCharSequence<T> matches(final CharSequence regex, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.matches(this.getResult(), regex, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> find(final Pattern pattern) {
        return this.find(pattern, null);
    }

    default PredicateStepCharSequence<T> find(final Pattern pattern, final CharSequence message, final Object... arguments) {
        return this.find(pattern, null, message, arguments);
    }

    default PredicateStepCharSequence<T> find(final Pattern pattern, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.find(this.getResult(), pattern, locale, message, arguments);
    }

    default PredicateStepCharSequence<T> find(final CharSequence regex) {
        return this.find(regex, null);
    }

    default PredicateStepCharSequence<T> find(final CharSequence regex, final CharSequence message, final Object... arguments) {
        return this.find(regex, null, message, arguments);
    }

    default PredicateStepCharSequence<T> find(final CharSequence regex, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.find(this.getResult(), regex, locale, message, arguments);
    }
}