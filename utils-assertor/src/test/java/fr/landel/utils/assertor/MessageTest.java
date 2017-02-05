/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import static org.junit.Assert.assertEquals;

import java.util.Locale;

import org.junit.Test;

/**
 * Check {@link Message}
 *
 * @since Aug 11, 2016
 * @author Gilles
 *
 */
public class MessageTest {

    /**
     * Check {@link Message#toString()}
     */
    @Test
    public void testToString() {
        assertEquals("[]", Message.of(null, null, null).toString());
        assertEquals("[locale: fr_FR]", Message.of(Locale.FRANCE, null, null).toString());
        assertEquals("[locale: fr_FR, message: message]", Message.of(Locale.FRANCE, "message", null).toString());
        assertEquals("[locale: fr_FR, arguments: arg1, arg2]", Message.of(Locale.FRANCE, null, new String[] {"arg1", "arg2"}).toString());
        assertEquals("[message: message]", Message.of(null, "message", null).toString());
        assertEquals("[locale: fr_FR, message: message, arguments: arg1, arg2]",
                Message.of(Locale.FRANCE, "message", new String[] {"arg1", "arg2"}).toString());
        assertEquals("[message: message, arguments: arg1, arg2]", Message.of(null, "message", new String[] {"arg1", "arg2"}).toString());
        assertEquals("[arguments: arg1, arg2]", Message.of(null, null, new String[] {"arg1", "arg2"}).toString());
    }
}
