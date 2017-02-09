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
 * Interface to manage listen events
 *
 * @since Dec 11, 2015
 * @author Gilles Landel
 *
 */
public interface Listener {
    /**
     * Execute the event
     * 
     * @param event
     *            The event
     */
    void execute(EventListener event);
}
