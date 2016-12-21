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

/**
 * (Description)
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepNumber<N extends Number & Comparable<N>> extends PredicateStep<PredicateStepNumber<N>, N> {

    default PredicateStepNumber<N> get(final StepAssertor<N> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorNumber<N> and() {
        return () -> HelperAssertor.and(this.getStep());
    }

    @Override
    default PredicateAssertorNumber<N> or() {
        return () -> HelperAssertor.or(this.getStep());
    }

    @Override
    default PredicateAssertorNumber<N> xor() {
        return () -> HelperAssertor.xor(this.getStep());
    }
}