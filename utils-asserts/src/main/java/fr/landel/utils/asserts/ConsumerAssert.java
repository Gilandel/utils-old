/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

/**
 * Throwable consumer assert
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <E>
 *            The expected exception type
 */
@FunctionalInterface
public interface ConsumerAssert<E extends Throwable> {

    /**
     * Assert that the code throws the specified exception.
     *
     * @throws E
     *             On error exception
     */
    void assertException() throws E;
}
