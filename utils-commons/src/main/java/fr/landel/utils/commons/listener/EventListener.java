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
package fr.landel.utils.commons.listener;

/**
 * Event listener
 *
 * @since Dec 11, 2015
 * @author Gilles Landel
 *
 */
public class EventListener {

    private int value;

    private Object source;

    /**
     * The constructor
     * 
     * @param value
     *            The event value
     * @param source
     *            The source object
     */
    public EventListener(final int value, final Object source) {
        this.value = value;
        this.source = source;
    }

    /**
     * @return the value
     */
    public int getValue() {
        return this.value;
    }

    /**
     * @return the source
     */
    public Object getSource() {
        return this.source;
    }
}
