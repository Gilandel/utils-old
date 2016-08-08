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
import java.util.function.Supplier;

import fr.landel.utils.commons.function.PredicateThrowable;

/**
 * This class define methods that can be applied on the checked object type. To
 * provide a result, it's also provide a chain builder by returning a
 * {@link PredicateStep}. The chain looks like:
 * 
 * <pre>
 * {@link PredicateAssertor} &gt; {@link PredicateStep} &gt; {@link PredicateAssertor} &gt; {@link PredicateStep}...
 * </pre>
 * 
 * This chain always starts with a {@link PredicateAssertor} and ends with
 * {@link PredicateStep}.
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 * @param <S>
 *            the type of predicate step
 * @param <T>
 *            the type of checked object
 */
@FunctionalInterface
public interface PredicateAssertor<S extends PredicateStep<S, T>, T> {

    Supplier<AssertorResult<T>> getStep();

    /**
     * The only purpose of this is to avoid the copy of basic methods into
     * children interfaces. This is an indirect way to create specific
     * {@link PredicateStep} by overriding this interface. All children class
     * has to override this method.
     * 
     * @param supplier
     *            The supplier
     * @return The predicate step
     */
    @SuppressWarnings("unchecked")
    default S get(final Supplier<AssertorResult<T>> supplier) {
        return (S) (PredicateStep<S, T>) () -> supplier;
    }

    /**
     * Apply the NOT operator on the next assertion
     * 
     * @return an assertor based on the current one
     */
    default PredicateAssertor<S, T> not() {
        return () -> AssertorHelper.not(this.getStep());
    }

    default S isNull() {
        return this.isNull(null);
    }

    default S isNull(final CharSequence message, final Object... arguments) {
        return this.isNull(null, message, arguments);
    }

    default S isNull(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.get(AssertorObject.isNull(this.getStep(), locale, message, arguments));
    }

    default S isNotNull() {
        return this.isNotNull(null);
    }

    default S isNotNull(final CharSequence message, final Object... arguments) {
        return this.isNotNull(null, message, arguments);
    }

    default S isNotNull(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.get(AssertorObject.isNotNull(this.getStep(), locale, message, arguments));
    }

    default S isEqual(final Object object) {
        return this.isEqual(object, null);
    }

    default S isEqual(final Object object, final CharSequence message, final Object... arguments) {
        return this.isEqual(object, null, message, arguments);
    }

    default S isEqual(final Object object, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.get(AssertorObject.isEqual(this.getStep(), object, locale, message, arguments));
    }

    default S isNotEqual(final Object object) {
        return this.isNotEqual(object, null);
    }

    default S isNotEqual(final Object object, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(object, null, message, arguments);
    }

    default S isNotEqual(final Object object, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.get(AssertorObject.isNotEqual(this.getStep(), object, locale, message, arguments));
    }

    default S isInstanceOf(final Class<?> clazz) {
        return this.isInstanceOf(clazz, null);
    }

    default S isInstanceOf(final Class<?> clazz, final CharSequence message, final Object... arguments) {
        return this.isInstanceOf(clazz, null, message, arguments);
    }

    default S isInstanceOf(final Class<?> clazz, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.get(AssertorObject.isInstanceOf(this.getStep(), clazz, locale, message, arguments));
    }

    default S isAssignableFrom(final Class<?> clazz) {
        return this.isAssignableFrom(clazz, null);
    }

    default S isAssignableFrom(final Class<?> clazz, final CharSequence message, final Object... arguments) {
        return this.isAssignableFrom(clazz, null, message, arguments);
    }

    default S isAssignableFrom(final Class<?> clazz, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.get(AssertorObject.isAssignableFrom(this.getStep(), clazz, locale, message, arguments));
    }

    default <E extends Throwable> S validates(final PredicateThrowable<T, E> predicate) {
        return this.validates(predicate, null);
    }

    default <E extends Throwable> S validates(final PredicateThrowable<T, E> predicate, final CharSequence message,
            final Object... arguments) {
        return this.validates(predicate, null, message, arguments);
    }

    default <E extends Throwable> S validates(final PredicateThrowable<T, E> predicate, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.get(AssertorObject.validates(this.getStep(), predicate, locale, message, arguments));
    }
}