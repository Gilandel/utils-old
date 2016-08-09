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
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorClass<T> extends PredicateAssertor<PredicateStepClass<T>, Class<T>> {

    @Override
    default PredicateStepClass<T> get(final AssertorResult<Class<T>> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorClass<T> not() {
        return () -> HelperAssertor.not(getResult());
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
        return () -> AssertorClass.isAssignableFrom(this.getResult(), clazz, locale, message, arguments);
    }
}