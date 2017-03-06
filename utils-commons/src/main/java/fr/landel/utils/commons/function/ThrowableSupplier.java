/*
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

/**
 * Represents an operation that accepts no input argument and returns no result.
 * The only output is through an exception. Unlike most other functional
 * interfaces, {@link ThrowableSupplier} is expected to operate via
 * side-effects.
 * 
 * <p>
 * This class can be used for example to prepare an exception or to assert the
 * good exception is thrown, see: fr.landel.utils.assertor.expect.Expect.
 * </p>
 *
 * <p>
 * This is a <a href="package-summary.html">functional interface</a> whose
 * functional method is {@link #throwException()}.
 * </p>
 * 
 * <pre>
 * ThrowableSupplier&lt;Exception&gt; exceptionBuilder = () -&gt; {
 *     throw new Exception("Not possible");
 * };
 * </pre>
 *
 * @since May 14, 2016
 * @author Gilles
 *
 * @param <E>
 *            The exception type
 */
@FunctionalInterface
public interface ThrowableSupplier<E extends Throwable> {

    /**
     * Throws the specified exception.
     *
     * @throws E
     *             On error exception
     */
    void throwException() throws E;
}
