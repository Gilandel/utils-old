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
 * 
 * (Description)
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 * @param <T>
 *            the type of the checked object
 */
@FunctionalInterface
public interface PredicateStepCharSequence<T extends CharSequence> extends PredicateStep<PredicateStepCharSequence<T>, T> {

    /**
     * {@inheritDoc}
     */
    @Override
    default PredicateStepCharSequence<T> get(final Supplier<AssertorResult<T>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorCharSequence<T> and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorCharSequence<T> or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorCharSequence<T> xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}