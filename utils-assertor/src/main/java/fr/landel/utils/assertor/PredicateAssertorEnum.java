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
 * 
 * (Description)
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorEnum<T extends Enum<T>> extends PredicateAssertor<PredicateStepEnum<T>, T> {

    @Override
    default PredicateStepEnum<T> get(final AssertorResult<T> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorEnum<T> not() {
        return () -> HelperAssertor.not(getResult());
    }

    default PredicateStepEnum<T> isName(final CharSequence name) {
        return this.isName(name, null);
    }

    default PredicateStepEnum<T> isName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.isName(name, null, message, arguments);
    }

    default PredicateStepEnum<T> isName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorEnum.isName(this.getResult(), name, locale, message, arguments);
    }

    default PredicateStepEnum<T> isNameIgnoreCase(final CharSequence name) {
        return this.isNameIgnoreCase(name, null);
    }

    default PredicateStepEnum<T> isNameIgnoreCase(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.isNameIgnoreCase(name, null, message, arguments);
    }

    default PredicateStepEnum<T> isNameIgnoreCase(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorEnum.isNameIgnoreCase(this.getResult(), name, locale, message, arguments);
    }

    default PredicateStepEnum<T> isOrdinal(final int ordinal) {
        return this.isOrdinal(ordinal, null);
    }

    default PredicateStepEnum<T> isOrdinal(final int ordinal, final CharSequence message, final Object... arguments) {
        return this.isOrdinal(ordinal, null, message, arguments);
    }

    default PredicateStepEnum<T> isOrdinal(final int ordinal, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorEnum.isOrdinal(this.getResult(), ordinal, locale, message, arguments);
    }
}