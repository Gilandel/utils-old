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

import java.util.Date;
import java.util.function.Supplier;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepDate extends PredicateStep<PredicateStepDate, Date> {

    default PredicateStepDate get(final Supplier<AssertorResult<Date>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorDate and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorDate or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorDate xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}