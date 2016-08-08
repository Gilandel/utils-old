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
public interface PredicateStepNumber<N extends Number & Comparable<N>> extends PredicateStep<PredicateStepNumber<N>, N> {

    default PredicateStepNumber<N> get(final Supplier<AssertorResult<N>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorNumber<N> and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorNumber<N> or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorNumber<N> xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}