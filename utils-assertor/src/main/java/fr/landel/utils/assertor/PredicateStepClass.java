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
public interface PredicateStepClass<T> extends PredicateStep<PredicateStepClass<T>, Class<T>> {

    default PredicateStepClass<T> get(final StepAssertor<Class<T>> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorClass<T> and() {
        return () -> HelperAssertor.and(this.getStep());
    }

    @Override
    default PredicateAssertorClass<T> or() {
        return () -> HelperAssertor.or(this.getStep());
    }

    @Override
    default PredicateAssertorClass<T> xor() {
        return () -> HelperAssertor.xor(this.getStep());
    }
}