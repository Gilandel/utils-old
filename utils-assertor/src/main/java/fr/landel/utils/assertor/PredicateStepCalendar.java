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

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepCalendar extends PredicateStep<PredicateStepCalendar, Calendar> {

    default PredicateStepCalendar get(final AssertorResult<Calendar> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorCalendar and() {
        return () -> HelperAssertor.and(this.getResult());
    }

    @Override
    default PredicateAssertorCalendar or() {
        return () -> HelperAssertor.or(this.getResult());
    }

    @Override
    default PredicateAssertorCalendar xor() {
        return () -> HelperAssertor.xor(this.getResult());
    }
}