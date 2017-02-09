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
package fr.landel.utils.commons.exception;

/**
 * Function exception is the exception thrown by throwable function
 *
 * @since Aug 2, 2016
 * @author Gilles
 *
 */
public class FunctionException extends AbstractRuntimeException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -60351467883322041L;

    /**
     * Constructs a new function exception with the specified cause and a detail
     * message of <tt>(cause==null ? null : cause.toString())</tt> (which
     * typically contains the class and detail message of <tt>cause</tt>). This
     * constructor is useful for runtime exceptions that are little more than
     * wrappers for other throwables.
     *
     * @param cause
     *            the cause (which is saved for later retrieval by the
     *            {@link #getCause()} method). (A <tt>null</tt> value is
     *            permitted, and indicates that the cause is nonexistent or
     *            unknown.)
     */
    public FunctionException(final Throwable cause) {
        super(cause);
    }
}
