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

import fr.landel.utils.commons.tuple.Quad;

/**
 * Represents a supplier of quad results.
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
 * @param <A>
 *            The first type result
 * @param <B>
 *            The second type result
 * @param <C>
 *            The third type result
 * @param <D>
 *            The fourth type result
 */
@FunctionalInterface
public interface QuadSupplier<A, B, C, D> extends Supplier<Quad<A, B, C, D>> {
}
