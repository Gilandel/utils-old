/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.commons.listener;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import fr.landel.utils.commons.listener.AbstractListenable;
import fr.landel.utils.commons.listener.EventListener;
import fr.landel.utils.commons.listener.Listener;

/**
 * Class to check the listenable design pattern
 *
 * @since 23 avr. 2016
 * @author Gilles
 *
 */
public class ListenerTest {

    private static int eventCount = 0;

    /**
     * The object to listen
     *
     * @since 23 avr. 2016
     * @author Gilles
     *
     */
    private class Listenable extends AbstractListenable {

    }

    /**
     * An object which listens the listenable
     *
     * @since 23 avr. 2016
     * @author Gilles
     *
     */
    private class ListenerImpl implements Listener {
        /**
         * Add event type
         */
        public static final int ADD_EVENT = 1;

        /**
         * Remove event type
         */
        public static final int REMOVE_EVENT = 2;

        @Override
        public void execute(final EventListener event) {
            switch (event.getValue()) {
            case ADD_EVENT:
                ListenerTest.eventCount++;
                break;
            case REMOVE_EVENT:
                ListenerTest.eventCount--;
                break;
            default:
            }
        }
    }

    @Test
    public void test() {
        final Listenable listenable = new Listenable();

        // register events
        listenable.addListener(new ListenerImpl());
        listenable.addListener(new ListenerImpl());

        // Send an event to the listenable, and the listenable sends itself an
        // event to all registered listeners
        listenable.fire(new EventListener(ListenerImpl.ADD_EVENT, this));

        assertEquals(2, eventCount);

        listenable.fire(new EventListener(ListenerImpl.REMOVE_EVENT, this));

        assertEquals(0, eventCount);
    }
}