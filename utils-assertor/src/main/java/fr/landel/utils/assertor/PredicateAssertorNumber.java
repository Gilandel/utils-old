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
public interface PredicateAssertorNumber<N extends Number & Comparable<N>> extends PredicateAssertor<PredicateStepNumber<N>, N> {

    @Override
    default PredicateStepNumber<N> get(final StepAssertor<N> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorNumber<N> not() {
        return () -> HelperAssertor.not(getStep());
    }

    default PredicateStepNumber<N> isEqual(final N number) {
        return this.isEqual(number, null);
    }

    default PredicateStepNumber<N> isEqual(final N number, final CharSequence message, final Object... arguments) {
        return this.isEqual(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isEqual(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isEqual(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isNotEqual(final N number) {
        return this.isNotEqual(number, null);
    }

    default PredicateStepNumber<N> isNotEqual(final N number, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isNotEqual(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isNotEqual(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isGT(final N number) {
        return this.isGT(number, null);
    }

    default PredicateStepNumber<N> isGT(final N number, final CharSequence message, final Object... arguments) {
        return this.isGT(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isGT(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isGT(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isGTE(final N number) {
        return this.isGTE(number, null);
    }

    default PredicateStepNumber<N> isGTE(final N number, final CharSequence message, final Object... arguments) {
        return this.isGTE(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isGTE(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isGTE(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isLT(final N number) {
        return this.isLT(number, null);
    }

    default PredicateStepNumber<N> isLT(final N number, final CharSequence message, final Object... arguments) {
        return this.isLT(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isLT(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isLT(this.getStep(), number, Message.of(locale, message, arguments));
    }

    default PredicateStepNumber<N> isLTE(final N number) {
        return this.isLTE(number, null);
    }

    default PredicateStepNumber<N> isLTE(final N number, final CharSequence message, final Object... arguments) {
        return this.isLTE(number, null, message, arguments);
    }

    default PredicateStepNumber<N> isLTE(final N number, final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorNumber.isLTE(this.getStep(), number, Message.of(locale, message, arguments));
    }
}