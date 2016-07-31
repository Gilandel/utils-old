/*
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
package fr.landel.utils.commons.function;

/**
 * Represents an operation that accepts no input argument and returns no result.
 * The only output is through an exception. Unlike most other functional
 * interfaces, {@code AssertConsumer} is expected to operate via side-effects.
 * 
 * <p>
 * This class can be used for example to prepare an exception or to assert the
 * good exception is thrown, see: fr.landel.utils.assertor.expect.Expect.
 * </p>
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #assertException()}.
 * </p>
 * 
 * <pre>
 * AssertConsumer&lt;Exception&gt; exceptionBuilder = () -&gt; {
 *     throw new Exception("Not possible");
 * };
 * </pre>
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface AssertConsumer<E extends Throwable> {

    /**
     * Assert that the code throws the specified exception.
     *
     * @throws E
     *             On error exception
     */
    void assertException() throws E;
}