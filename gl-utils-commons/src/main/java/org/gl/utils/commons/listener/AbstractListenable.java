/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * 
 * This code is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons.listener;

import java.util.HashSet;
import java.util.Set;

/**
 * Asbtract listenable
 *
 * @since 11 déc. 2015
 * @author Gilles Landel
 *
 */
public abstract class AbstractListenable {

    private Set<Listener> listeners;

    /**
     * Constructor
     */
    public AbstractListenable() {
        this.listeners = new HashSet<>();
    }

    /**
     * Fire the event to all listeners
     * 
     * @param event
     *            The event to send to listeners
     */
    protected void fire(final EventListener event) {
        for (Listener listener : this.listeners) {
            listener.execute(event);
        }
    }

    /**
     * Add a listener
     * 
     * @param listener
     *            The new listener
     */
    public void addListener(final Listener listener) {
        this.listeners.add(listener);
    }
}
