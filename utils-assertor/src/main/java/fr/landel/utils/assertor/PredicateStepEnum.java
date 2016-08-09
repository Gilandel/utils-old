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
public interface PredicateStepEnum<T extends Enum<T>> extends PredicateStep<PredicateStepEnum<T>, T> {

    default PredicateStepEnum<T> get(final AssertorResult<T> result) {
        return () -> result;
    }

    @Override
    default PredicateAssertorEnum<T> and() {
        return () -> HelperAssertor.and(this.getResult());
    }

    @Override
    default PredicateAssertorEnum<T> or() {
        return () -> HelperAssertor.or(this.getResult());
    }

    @Override
    default PredicateAssertorEnum<T> xor() {
        return () -> HelperAssertor.xor(this.getResult());
    }
}