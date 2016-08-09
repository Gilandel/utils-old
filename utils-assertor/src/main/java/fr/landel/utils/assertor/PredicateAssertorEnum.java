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

    default PredicateStepEnum<T> hasName(final CharSequence name) {
        return this.hasName(name, null);
    }

    default PredicateStepEnum<T> hasName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasName(name, null, message, arguments);
    }

    default PredicateStepEnum<T> hasName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorEnum.hasName(this.getResult(), name, locale, message, arguments);
    }

    default PredicateStepEnum<T> hasNameIgnoreCase(final CharSequence name) {
        return this.hasNameIgnoreCase(name, null);
    }

    default PredicateStepEnum<T> hasNameIgnoreCase(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasNameIgnoreCase(name, null, message, arguments);
    }

    default PredicateStepEnum<T> hasNameIgnoreCase(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorEnum.hasNameIgnoreCase(this.getResult(), name, locale, message, arguments);
    }

    default PredicateStepEnum<T> hasOrdinal(final int ordinal) {
        return this.hasOrdinal(ordinal, null);
    }

    default PredicateStepEnum<T> hasOrdinal(final int ordinal, final CharSequence message, final Object... arguments) {
        return this.hasOrdinal(ordinal, null, message, arguments);
    }

    default PredicateStepEnum<T> hasOrdinal(final int ordinal, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorEnum.hasOrdinal(this.getResult(), ordinal, locale, message, arguments);
    }
}