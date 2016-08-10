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
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepArray<T> extends PredicateStep<PredicateStepArray<T>, T[]> {

    default PredicateStepArray<T> get(final StepAssertor<T[]> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorArray<T> and() {
        return () -> HelperAssertor.and(this.getStep());
    }

    @Override
    default PredicateAssertorArray<T> or() {
        return () -> HelperAssertor.or(this.getStep());
    }

    @Override
    default PredicateAssertorArray<T> xor() {
        return () -> HelperAssertor.xor(this.getStep());
    }
}