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

import java.util.function.Supplier;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepBoolean extends PredicateStep<PredicateStepBoolean, Boolean> {

    default PredicateStepBoolean get(final Supplier<AssertorResult<Boolean>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorBoolean and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorBoolean or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorBoolean xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}