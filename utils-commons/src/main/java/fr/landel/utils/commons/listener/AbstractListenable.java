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

import java.util.HashSet;
import java.util.Set;

/**
 * Asbtract listenable
 *
 * @since Dec 11, 2015
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
    public void fire(final EventListener event) {
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
