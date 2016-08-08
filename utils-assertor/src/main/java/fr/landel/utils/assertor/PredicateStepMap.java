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

import java.util.Map;
import java.util.function.Supplier;

/**
 * (Description)
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
@FunctionalInterface
public interface PredicateStepMap<K, V> extends PredicateStep<PredicateStepMap<K, V>, Map<K, V>> {

    default PredicateStepMap<K, V> get(final Supplier<AssertorResult<Map<K, V>>> supplier) {
        return () -> supplier;
    }

    @Override
    default PredicateAssertorMap<K, V> and() {
        return () -> AssertorHelper.and(this.getStep());
    }

    @Override
    default PredicateAssertorMap<K, V> or() {
        return () -> AssertorHelper.or(this.getStep());
    }

    @Override
    default PredicateAssertorMap<K, V> xor() {
        return () -> AssertorHelper.xor(this.getStep());
    }
}