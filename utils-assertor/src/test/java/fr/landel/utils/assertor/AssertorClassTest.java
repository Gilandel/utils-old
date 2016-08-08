/*-
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.awt.Color;
import java.io.IOException;
import java.util.Locale;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check {@link AssertorClass}
 *
 * @since 18 juil. 2016
 * @author Gilles
 *
 */
public class AssertorClassTest extends AbstractTest {

    /**
     * Test method for {@link AssertorClass#AssertorClass()} .
     */
    @Test
    public void testConstructor() {
        assertNotNull(new AssertorClass());
    }

    /**
     * Test method for {@link AssertorClass#isAssignableFrom} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test
    public void testIsAssignableFrom() throws IOException {
        assertTrue(Assertor.that(IOException.class).isAssignableFrom(Exception.class).isOK());
        assertTrue(Assertor.that(IOException.class).isAssignableFrom(Exception.class, "test").isOK());
        assertTrue(Assertor.that(IOException.class).isAssignableFrom(Exception.class, Locale.US, "test %2d", 12).isOK());

        assertTrue(Assertor.that(IOException.class).not().isAssignableFrom(Color.class).isOK());
        assertTrue(Assertor.that(IOException.class).not().isNull().isOK());

        assertTrue(Assertor.that(IOException.class).isNotNull().and().not().isAssignableFrom(Color.class).isOK());
        assertTrue(Assertor.that(IOException.class).isNotNull().or().isAssignableFrom(Color.class).isOK());
        assertTrue(Assertor.that(IOException.class).isNotNull().xor().isAssignableFrom(Color.class).isOK());

        assertTrue(Assertor.that(IOException.class).isNotNull().and(Assertor.that(true).isTrue()).and().not().isAssignableFrom(Color.class)
                .isOK());

        Expect.exception(() -> {
            Assertor.that(Exception.class).isAssignableFrom(IOException.class).toThrow();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(Exception.class).isAssignableFrom(IOException.class).toThrow("msg");
        }, IllegalArgumentException.class, "msg");

        Expect.exception(() -> {
            Assertor.that(Exception.class).isAssignableFrom(null).toThrow("msg");
        }, IllegalArgumentException.class, "msg");

        Expect.exception(() -> {
            Assertor.that((Class<?>) null).isAssignableFrom(null).toThrow("msg");
        }, IllegalArgumentException.class, "msg");

        Expect.exception(() -> {
            Assertor.that((Class<?>) null).isAssignableFrom(Exception.class).toThrow("msg");
        }, IllegalArgumentException.class, "msg");
    }
}
