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

/**
 * 
 * (Description)
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateAssertorBoolean extends PredicateAssertor<PredicateStepBoolean, Boolean> {

    @Override
    default PredicateStepBoolean get(final Supplier<AssertorResult<Boolean>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorBoolean not() {
        return () -> AssertorHelper.not(getStep());
    }

    default PredicateStepBoolean isTrue() {
        return this.isTrue(null);
    }

    default PredicateStepBoolean isTrue(final CharSequence message, final Object... arguments) {
        return this.isTrue(null, message, arguments);
    }

    default PredicateStepBoolean isTrue(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorBoolean.isTrue(this.getStep(), locale, message, arguments);
    }

    default PredicateStepBoolean isFalse() {
        return this.isFalse(null);
    }

    default PredicateStepBoolean isFalse(final CharSequence message, final Object... arguments) {
        return this.isFalse(null, message, arguments);
    }

    default PredicateStepBoolean isFalse(final Locale locale, final CharSequence message, final Object... arguments) {
        return () -> AssertorBoolean.isFalse(this.getStep(), locale, message, arguments);
    }
}