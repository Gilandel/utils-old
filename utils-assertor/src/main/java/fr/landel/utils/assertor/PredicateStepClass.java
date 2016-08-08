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
public interface PredicateStepClass<T> extends PredicateStep<PredicateStepClass<T>, Class<T>> {

    default PredicateStepClass<T> get(final Supplier<AssertorResult<Class<T>>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorClass<T> and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorClass<T> or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorClass<T> xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}