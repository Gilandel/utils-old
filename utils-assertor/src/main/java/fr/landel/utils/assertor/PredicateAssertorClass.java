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
 * (Description)
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorClass<T> extends PredicateAssertor<PredicateStepClass<T>, Class<T>> {

    @Override
    default PredicateStepClass<T> get(final StepAssertor<Class<T>> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorClass<T> not() {
        return () -> HelperAssertor.not(getStep());
    }

    @Override
    default PredicateStepClass<T> isAssignableFrom(final Class<?> clazz) {
        return this.isAssignableFrom(clazz, null);
    }

    @Override
    default PredicateStepClass<T> isAssignableFrom(final Class<?> clazz, final CharSequence message, final Object... arguments) {
        return this.isAssignableFrom(clazz, null, message, arguments);
    }

    @Override
    default PredicateStepClass<T> isAssignableFrom(final Class<?> clazz, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorClass.isAssignableFrom(this.getStep(), clazz, Message.of(locale, message, arguments));
    }

    default PredicateStepClass<T> hasName(final CharSequence name) {
        return this.hasName(name, null);
    }

    default PredicateStepClass<T> hasName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasName(name, null, message, arguments);
    }

    default PredicateStepClass<T> hasName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorClass.hasName(this.getStep(), name, Message.of(locale, message, arguments));
    }

    default PredicateStepClass<T> hasSimpleName(final CharSequence name) {
        return this.hasSimpleName(name, null);
    }

    default PredicateStepClass<T> hasSimpleName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasSimpleName(name, null, message, arguments);
    }

    default PredicateStepClass<T> hasSimpleName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorClass.hasSimpleName(this.getStep(), name, Message.of(locale, message, arguments));
    }

    default PredicateStepClass<T> hasCanonicalName(final CharSequence name) {
        return this.hasCanonicalName(name, null);
    }

    default PredicateStepClass<T> hasCanonicalName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasCanonicalName(name, null, message, arguments);
    }

    default PredicateStepClass<T> hasCanonicalName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorClass.hasCanonicalName(this.getStep(), name, Message.of(locale, message, arguments));
    }

    default PredicateStepClass<T> hasTypeName(final CharSequence name) {
        return this.hasTypeName(name, null);
    }

    default PredicateStepClass<T> hasTypeName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasTypeName(name, null, message, arguments);
    }

    default PredicateStepClass<T> hasTypeName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorClass.hasTypeName(this.getStep(), name, Message.of(locale, message, arguments));
    }

    default PredicateStepClass<T> hasPackageName(final CharSequence name) {
        return this.hasPackageName(name, null);
    }

    default PredicateStepClass<T> hasPackageName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasPackageName(name, null, message, arguments);
    }

    default PredicateStepClass<T> hasPackageName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorClass.hasPackageName(this.getStep(), name, Message.of(locale, message, arguments));
    }
}