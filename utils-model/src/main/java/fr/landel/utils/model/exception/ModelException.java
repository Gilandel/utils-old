/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.exception;

import fr.landel.utils.commons.exception.AbstractException;

/**
 * The model exception
 *
 * @since 24 nov. 2015
 * @author Gilles
 *
 */
public class ModelException extends AbstractException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -5088316000492567608L;

    /**
     * Constructor.
     * 
     */
    public ModelException() {
        super();
    }

    /**
     * Constructor with message.
     * 
     * @param message
     *            message
     */
    public ModelException(final String message) {
        super(message);
    }

    /**
     * Constructor with exception.
     * 
     * @param exception
     *            The exception
     */
    public ModelException(final Throwable exception) {
        super(ModelException.class, exception);
    }

    /**
     * Constructor with message and exception.
     * 
     * @param message
     *            The message
     * @param exception
     *            The exception
     */
    public ModelException(final String message, final Throwable exception) {
        super(message, exception);
    }
}
