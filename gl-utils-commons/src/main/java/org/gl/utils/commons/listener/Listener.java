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

/**
 * Interface to manage listen events
 *
 * @since 11 déc. 2015
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
