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
package fr.landel.utils.commons.exception;

/**
 * Illegal operation exception, extends {@link RuntimeException}.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public class IllegalOperationException extends AbstractRuntimeException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 5900016009511270329L;

    /**
     * Constructor
     */
    public IllegalOperationException() {
        super();
    }

    /**
     * Constructor with message.
     * 
     * @param message
     *            message
     */
    public IllegalOperationException(final CharSequence message) {
        super(message);
    }

    /**
     * Constructor.
     *
     * @param cause
     *            The cause
     */
    public IllegalOperationException(final Throwable cause) {
        super(cause);
    }

    /**
     * Constructor with class and exception.
     * 
     * @param clazz
     *            The super class
     * @param exception
     *            The exception
     */
    public IllegalOperationException(final Class<? extends IllegalOperationException> clazz, final Throwable exception) {
        super(clazz, exception);
    }

    /**
     * Constructor with message and exception.
     * 
     * @param message
     *            The message
     * @param exception
     *            The exception
     */
    public IllegalOperationException(final CharSequence message, final Throwable exception) {
        super(message, exception);
    }

    /**
     * 
     * Constructor.
     *
     * @param message
     *            The message
     * @param cause
     *            the cause
     * @param enableSuppression
     *            whether or not suppression is enabled or disabled
     * @param writableStackTrace
     *            whether or not the stack trace should be writable
     */
    protected IllegalOperationException(final CharSequence message, final Throwable cause, final boolean enableSuppression,
            final boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
