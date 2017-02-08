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
 * This class define methods that can be applied on the checked {@link Enum}
 * object. To provide a result, it's also provide a chain builder by returning a
 * {@link PredicateStepEnum}. The chain looks like:
 * 
 * <pre>
 * {@link PredicateAssertorEnum} &gt; {@link PredicateStepEnum} &gt; {@link PredicateAssertorEnum} &gt; {@link PredicateStepEnum}...
 * </pre>
 * 
 * This chain always starts with a {@link PredicateAssertorEnum} and ends with
 * {@link PredicateStepEnum}.
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorEnum<T extends Enum<T>> extends PredicateAssertor<PredicateStepEnum<T>, T> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepEnum<T> get(final StepAssertor<T> result) {
        return () -> result;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateAssertorEnum<T> not() {
        return () -> HelperAssertor.not(getStep());
    }

    default PredicateStepEnum<T> hasName(final CharSequence name) {
        return this.hasName(name, null);
    }

    default PredicateStepEnum<T> hasName(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasName(name, null, message, arguments);
    }

    default PredicateStepEnum<T> hasName(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorEnum.hasName(this.getStep(), name, MessageAssertor.of(locale, message, arguments));
    }

    default PredicateStepEnum<T> hasNameIgnoreCase(final CharSequence name) {
        return this.hasNameIgnoreCase(name, null);
    }

    default PredicateStepEnum<T> hasNameIgnoreCase(final CharSequence name, final CharSequence message, final Object... arguments) {
        return this.hasNameIgnoreCase(name, null, message, arguments);
    }

    default PredicateStepEnum<T> hasNameIgnoreCase(final CharSequence name, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return () -> AssertorEnum.hasNameIgnoreCase(this.getStep(), name, MessageAssertor.of(locale, message, arguments));
    }

    default PredicateStepEnum<T> hasOrdinal(final int ordinal) {
        return this.hasOrdinal(ordinal, null);
    }

    default PredicateStepEnum<T> hasOrdinal(final int ordinal, final CharSequence message, final Object... arguments) {
        return this.hasOrdinal(ordinal, null, message, arguments);
    }

    default PredicateStepEnum<T> hasOrdinal(final int ordinal, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorEnum.hasOrdinal(this.getStep(), ordinal, MessageAssertor.of(locale, message, arguments));
    }
}
