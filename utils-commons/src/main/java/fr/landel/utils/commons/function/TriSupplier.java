/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.function;

import java.util.function.Supplier;

import org.apache.commons.lang3.tuple.Triple;

/**
 * Represents a supplier of triple results.
 *
 * <p>
 * There is no requirement that a new or distinct result be returned each time
 * the supplier is invoked.
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #get()}.
 *
 * @since Aug 10, 2016
 * @author Gilles
 *
 * @param <L>
 *            The left type result
 * @param <M>
 *            The middle type result
 * @param <R>
 *            The right type result
 */
@FunctionalInterface
public interface TriSupplier<L, M, R> extends Supplier<Triple<L, M, R>> {
}
