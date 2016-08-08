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
public interface PredicateStepEnum<T extends Enum<T>> extends PredicateStep<PredicateStepEnum<T>, T> {

    default PredicateStepEnum<T> get(final Supplier<AssertorResult<T>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorEnum<T> and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorEnum<T> or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorEnum<T> xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}