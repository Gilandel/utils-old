/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.mapper;

import fr.landel.utils.commons.exception.AbstractException;

/**
 * The mapper exception
 *
 * @since Nov 24, 2015
 * @author Gilles
 *
 */
public class MapperException extends AbstractException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -5088326000492567608L;

    /**
     * Constructor.
     * 
     */
    public MapperException() {
        super();
    }

    /**
     * Constructor with message.
     * 
     * @param message
     *            message
     */
    public MapperException(final String message) {
        super(message);
    }

    /**
     * Constructor with exception.
     * 
     * @param exception
     *            The exception
     */
    public MapperException(final Throwable exception) {
        super(MapperException.class, exception);
    }

    /**
     * Constructor with message and exception.
     * 
     * @param message
     *            The message
     * @param exception
     *            The exception
     */
    public MapperException(final String message, final Throwable exception) {
        super(message, exception);
    }
}
