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

import java.util.Calendar;
import java.util.function.Supplier;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepCalendar extends PredicateStep<PredicateStepCalendar, Calendar> {

    default PredicateStepCalendar get(final Supplier<AssertorResult<Calendar>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorCalendar and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorCalendar or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorCalendar xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}