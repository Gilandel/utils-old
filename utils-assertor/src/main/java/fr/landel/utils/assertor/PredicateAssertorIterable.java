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

/**
 * This class define methods that can be applied on the checked {@link Iterable}
 * object. To provide a result, it's also provide a chain builder by returning a
 * {@link PredicateStepIterable}. The chain looks like:
 * 
 * <pre>
 * {@link PredicateAssertorIterable} &gt; {@link PredicateStepIterable} &gt; {@link PredicateAssertorIterable} &gt; {@link PredicateStepIterable}...
 * </pre>
 * 
 * This chain always starts with a {@link PredicateAssertorIterable} and ends
 * with {@link PredicateStepIterable}.
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorIterable<T> extends PredicateAssertor<PredicateStepIterable<T>, Iterable<T>> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepIterable<T> get(final StepAssertor<Iterable<T>> result) {
        return () -> result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateAssertorIterable<T> not() {
        return () -> HelperAssertor.not(getStep());
    }

    default PredicateStepIterable<T> hasSize(final int size) {
        return this.hasSize(size, null);
    }

    default PredicateStepIterable<T> hasSize(final int size, final CharSequence message, final Object... arguments) {
        return this.hasSize(size, null, message, arguments);
    }

    default PredicateStepIterable<T> hasSize(final int size, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorIterable.hasSize(this.getStep(), size, Message.of(locale, message, arguments));
    }

    default PredicateStepIterable<T> isEmpty() {
        return this.isEmpty(null);
    }

    default PredicateStepIterable<T> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    default PredicateStepIterable<T> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorIterable.isEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepIterable<T> isNotEmpty() {
        return this.isNotEmpty(null);
    }

    default PredicateStepIterable<T> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    default PredicateStepIterable<T> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorIterable.isNotEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepIterable<T> contains(final T value) {
        return this.contains(value, null);
    }

    default PredicateStepIterable<T> contains(final T value, final CharSequence message, final Object... arguments) {
        return this.contains(value, (Locale) null, message, arguments);
    }

    default PredicateStepIterable<T> contains(final T value, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorIterable.contains(this.getStep(), value, Message.of(locale, message, arguments));
    }

    default PredicateStepIterable<T> containsAll(final Iterable<T> values) {
        return this.containsAll(values, null);
    }

    default PredicateStepIterable<T> containsAll(final Iterable<T> values, final CharSequence message, final Object... arguments) {
        return this.containsAll(values, null, message, arguments);
    }

    default PredicateStepIterable<T> containsAll(final Iterable<T> values, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorIterable.containsAll(this.getStep(), values, Message.of(locale, message, arguments));
    }

    default PredicateStepIterable<T> containsAny(final Iterable<T> values) {
        return this.containsAny(values, null);
    }

    default PredicateStepIterable<T> containsAny(final Iterable<T> values, final CharSequence message, final Object... arguments) {
        return this.containsAny(values, null, message, arguments);
    }

    default PredicateStepIterable<T> containsAny(final Iterable<T> values, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorIterable.containsAny(this.getStep(), values, Message.of(locale, message, arguments));
    }
}
