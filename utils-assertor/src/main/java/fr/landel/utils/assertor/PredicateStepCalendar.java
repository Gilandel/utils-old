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

import java.util.Calendar;

/**
 * (Description)
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepCalendar extends PredicateStep<PredicateStepCalendar, Calendar> {

    default PredicateStepCalendar get(final StepAssertor<Calendar> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorCalendar and() {
        return () -> HelperAssertor.and(this.getStep());
    }

    @Override
    default PredicateAssertorCalendar or() {
        return () -> HelperAssertor.or(this.getStep());
    }

    @Override
    default PredicateAssertorCalendar xor() {
        return () -> HelperAssertor.xor(this.getStep());
    }
}
