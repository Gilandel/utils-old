/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
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
 * This class define methods that can be applied on the checked
 * {@link CharSequence} object. To provide a result, it's also provide a chain
 * builder by returning a {@link PredicateStepCharSequence}. The chain looks
 * like:
 * 
 * <pre>
 * {@link PredicateAssertorCharSequence} &gt; {@link PredicateStepCharSequence} &gt; {@link PredicateAssertorCharSequence} &gt; {@link PredicateStepCharSequence}...
 * </pre>
 * 
 * This chain always starts with a {@link PredicateAssertorCharSequence} and
 * ends with {@link PredicateStepCharSequence}.
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 * @param <T>
 *            The type of checked object
 */
@FunctionalInterface
public interface PredicateAssertorCharSequence<T extends CharSequence> extends PredicateAssertor<PredicateStepCharSequence<T>, T> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepCharSequence<T> get(final StepAssertor<T> result) {
        return () -> result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateAssertorCharSequence<T> not() {
        return () -> HelperAssertor.not(this.getStep());
    }

    /**
     * Asserts that the given char sequence has the specified length. The input
     * cannot not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be null and {@code length} must
     * be a positive number
     * </p>
     * 
     * <pre>
     * Assertor.that(name).hasLength(5).toThrow();
     * </pre>
     * 
     * @param length
     *            The length (cannot be lower than 0)
     * @return The operator
     */
    default PredicateStepCharSequence<T> hasLength(final int length) {
        return this.hasLength(length, null);
    }

    /**
     * Asserts that the given char sequence has the specified length. The input
     * cannot not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be null and {@code length} must
     * be a positive number
     * </p>
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
     * @return The operator
     */
    default PredicateStepCharSequence<T> hasLength(final int length, final CharSequence message, final Object... arguments) {
        return this.hasLength(length, null, message, arguments);
    }

    /**
     * Asserts that the given char sequence has the specified length. The input
     * cannot not be {@code null} and the length cannot be lower than 0 (returns
     * false).
     * 
     * <p>
     * precondition: {@link CharSequence} cannot be null and {@code length} must
     * be a positive number
     * </p>
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
     * @return The operator
     */
    default PredicateStepCharSequence<T> hasLength(final int length, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.hasLength(this.getStep(), length, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isEmpty() {
        return this.isEmpty(null);
    }

    default PredicateStepCharSequence<T> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isNotEmpty() {
        return this.isNotEmpty(null);
    }

    default PredicateStepCharSequence<T> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isNotEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isBlank() {
        return this.isBlank(null);
    }

    default PredicateStepCharSequence<T> isBlank(final CharSequence message, final Object... arguments) {
        return this.isBlank(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isBlank(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isBlank(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isNotBlank() {
        return this.isNotBlank(null);
    }

    default PredicateStepCharSequence<T> isNotBlank(final CharSequence message, final Object... arguments) {
        return this.isNotBlank(null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotBlank(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isNotBlank(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isEqual(final CharSequence substring) {
        return this.isEqual(substring, null);
    }

    default PredicateStepCharSequence<T> isEqual(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.isEqual(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isEqual(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.isEqual(this.getStep(), substring, false, false, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isEqualIgnoreCase(final CharSequence substring) {
        return this.isEqualIgnoreCase(substring, null);
    }

    default PredicateStepCharSequence<T> isEqualIgnoreCase(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.isEqualIgnoreCase(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isEqualIgnoreCase(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.isEqual(this.getStep(), substring, true, false, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isEqualIgnoreLineReturns(final CharSequence substring) {
        return this.isEqualIgnoreLineReturns(substring, null);
    }

    default PredicateStepCharSequence<T> isEqualIgnoreLineReturns(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.isEqualIgnoreLineReturns(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isEqualIgnoreLineReturns(final CharSequence substring, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isEqual(this.getStep(), substring, false, true, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isEqualIgnoreCaseAndLineReturns(final CharSequence substring) {
        return this.isEqualIgnoreCaseAndLineReturns(substring, null);
    }

    default PredicateStepCharSequence<T> isEqualIgnoreCaseAndLineReturns(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.isEqualIgnoreCaseAndLineReturns(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isEqualIgnoreCaseAndLineReturns(final CharSequence substring, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isEqual(this.getStep(), substring, true, true, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isNotEqual(final CharSequence substring) {
        return this.isNotEqual(substring, null);
    }

    default PredicateStepCharSequence<T> isNotEqual(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEqual(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.isNotEqual(this.getStep(), substring, false, false, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreCase(final CharSequence substring) {
        return this.isNotEqualIgnoreCase(substring, null);
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreCase(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.isNotEqualIgnoreCase(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreCase(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.isNotEqual(this.getStep(), substring, true, false, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreLineReturns(final CharSequence substring) {
        return this.isNotEqualIgnoreLineReturns(substring, null);
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreLineReturns(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.isNotEqualIgnoreLineReturns(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreLineReturns(final CharSequence substring, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isNotEqual(this.getStep(), substring, false, true, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreCaseAndLineReturns(final CharSequence substring) {
        return this.isNotEqualIgnoreCaseAndLineReturns(substring, null);
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreCaseAndLineReturns(final CharSequence substring, final CharSequence message,
            final Object... arguments) {
        return this.isNotEqualIgnoreCaseAndLineReturns(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> isNotEqualIgnoreCaseAndLineReturns(final CharSequence substring, final Locale locale,
            final CharSequence message, final Object... arguments) {
        return () -> AssertorCharSequence.isNotEqual(this.getStep(), substring, true, true, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> contains(final Character character) {
        return this.contains(character, null);
    }

    default PredicateStepCharSequence<T> contains(final Character character, final CharSequence message, final Object... arguments) {
        return this.contains(character, null, message, arguments);
    }

    default PredicateStepCharSequence<T> contains(final Character character, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.contains(this.getStep(), character, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> contains(final CharSequence substring) {
        return this.contains(substring, null);
    }

    default PredicateStepCharSequence<T> contains(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.contains(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> contains(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.contains(this.getStep(), substring, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> startsWith(final CharSequence substring) {
        return this.startsWith(substring, null);
    }

    default PredicateStepCharSequence<T> startsWith(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.startsWith(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> startsWith(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.startsWith(this.getStep(), substring, Message.of(locale, message, arguments));
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
        return () -> AssertorCharSequence.startsWithIgnoreCase(this.getStep(), substring, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> endsWith(final CharSequence substring) {
        return this.endsWith(substring, null);
    }

    default PredicateStepCharSequence<T> endsWith(final CharSequence substring, final CharSequence message, final Object... arguments) {
        return this.endsWith(substring, null, message, arguments);
    }

    default PredicateStepCharSequence<T> endsWith(final CharSequence substring, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.endsWith(this.getStep(), substring, Message.of(locale, message, arguments));
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
        return () -> AssertorCharSequence.endsWithIgnoreCase(this.getStep(), substring, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> matches(final Pattern pattern) {
        return this.matches(pattern, null);
    }

    default PredicateStepCharSequence<T> matches(final Pattern pattern, final CharSequence message, final Object... arguments) {
        return this.matches(pattern, null, message, arguments);
    }

    default PredicateStepCharSequence<T> matches(final Pattern pattern, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.matches(this.getStep(), pattern, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> matches(final CharSequence regex) {
        return this.matches(regex, null);
    }

    default PredicateStepCharSequence<T> matches(final CharSequence regex, final CharSequence message, final Object... arguments) {
        return this.matches(regex, null, message, arguments);
    }

    default PredicateStepCharSequence<T> matches(final CharSequence regex, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.matches(this.getStep(), regex, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> find(final Pattern pattern) {
        return this.find(pattern, null);
    }

    default PredicateStepCharSequence<T> find(final Pattern pattern, final CharSequence message, final Object... arguments) {
        return this.find(pattern, null, message, arguments);
    }

    default PredicateStepCharSequence<T> find(final Pattern pattern, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.find(this.getStep(), pattern, Message.of(locale, message, arguments));
    }

    default PredicateStepCharSequence<T> find(final CharSequence regex) {
        return this.find(regex, null);
    }

    default PredicateStepCharSequence<T> find(final CharSequence regex, final CharSequence message, final Object... arguments) {
        return this.find(regex, null, message, arguments);
    }

    default PredicateStepCharSequence<T> find(final CharSequence regex, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorCharSequence.find(this.getStep(), regex, Message.of(locale, message, arguments));
    }
}
