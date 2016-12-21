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

/**
 * This class define methods that can be applied on the checked Array object. To
 * provide a result, it's also provide a chain builder by returning a
 * {@link PredicateStepArray}. The chain looks like:
 * 
 * <pre>
 * {@link PredicateAssertorArray} &gt; {@link PredicateStepArray} &gt; {@link PredicateAssertorArray} &gt; {@link PredicateStepArray}...
 * </pre>
 * 
 * This chain always starts with a {@link PredicateAssertorArray} and ends with
 * {@link PredicateStepArray}.
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorArray<T> extends PredicateAssertor<PredicateStepArray<T>, T[]> {

    @Override
    default PredicateStepArray<T> get(final StepAssertor<T[]> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorArray<T> not() {
        return () -> HelperAssertor.not(getStep());
    }

    default PredicateStepArray<T> hasLength(final int length) {
        return this.hasLength(length, null);
    }

    default PredicateStepArray<T> hasLength(final int length, final CharSequence message, final Object... arguments) {
        return this.hasLength(length, null, message, arguments);
    }

    default PredicateStepArray<T> hasLength(final int length, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorArray.hasLength(this.getStep(), length, Message.of(locale, message, arguments));
    }

    default PredicateStepArray<T> isEmpty() {
        return this.isEmpty(null);
    }

    default PredicateStepArray<T> isEmpty(final CharSequence message, final Object... arguments) {
        return this.isEmpty(null, message, arguments);
    }

    default PredicateStepArray<T> isEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorArray.isEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepArray<T> isNotEmpty() {
        return this.isNotEmpty(null);
    }

    default PredicateStepArray<T> isNotEmpty(final CharSequence message, final Object... arguments) {
        return this.isNotEmpty(null, message, arguments);
    }

    default PredicateStepArray<T> isNotEmpty(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorArray.isNotEmpty(this.getStep(), Message.of(locale, message, arguments));
    }

    default PredicateStepArray<T> contains(final T object) {
        return this.contains(object, null);
    }

    default PredicateStepArray<T> contains(final T object, final CharSequence message, final Object... arguments) {
        return this.contains(object, null, message, arguments);
    }

    default PredicateStepArray<T> contains(final T object, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorArray.contains(this.getStep(), object, Message.of(locale, message, arguments));
    }

    default PredicateStepArray<T> containsAll(final T[] objects) {
        return this.containsAll(objects, null);
    }

    default PredicateStepArray<T> containsAll(final T[] objects, final CharSequence message, final Object... arguments) {
        return this.containsAll(objects, null, message, arguments);
    }

    default PredicateStepArray<T> containsAll(final T[] objects, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorArray.containsAll(this.getStep(), objects, Message.of(locale, message, arguments));
    }

    default PredicateStepArray<T> containsAny(final T[] objects) {
        return this.containsAny(objects, null);
    }

    default PredicateStepArray<T> containsAny(final T[] objects, final CharSequence message, final Object... arguments) {
        return this.containsAny(objects, null, message, arguments);
    }

    default PredicateStepArray<T> containsAny(final T[] objects, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorArray.containsAny(this.getStep(), objects, Message.of(locale, message, arguments));
    }
}