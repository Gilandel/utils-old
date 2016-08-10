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
public interface PredicateStepBoolean extends PredicateStep<PredicateStepBoolean, Boolean> {

    default PredicateStepBoolean get(final StepAssertor<Boolean> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorBoolean and() {
        return () -> HelperAssertor.and(this.getStep());
    }

    @Override
    default PredicateAssertorBoolean or() {
        return () -> HelperAssertor.or(this.getStep());
    }

    @Override
    default PredicateAssertorBoolean xor() {
        return () -> HelperAssertor.xor(this.getStep());
    }
}