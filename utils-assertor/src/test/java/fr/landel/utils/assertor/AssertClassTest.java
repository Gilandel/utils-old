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

import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Locale;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check assert class
 *
 * @since 18 juil. 2016
 * @author Gilles
 *
 */
public class AssertClassTest extends AbstractTest {

    /**
     * Test method for {@link AssertClass#isAssignableFrom} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test
    public void testIsAssignableFrom() throws IOException {
        assertTrue(Assertor.that(IOException.class).isAssignableFrom(Exception.class).isOK());
        assertTrue(Assertor.that(IOException.class).isAssignableFrom(Exception.class, "test").isOK());
        assertTrue(Assertor.that(IOException.class).isAssignableFrom(Exception.class, Locale.US, "test %2d", 12).isOK());

        Expect.exception(() -> {
            Assertor.that(Exception.class).isAssignableFrom(IOException.class).toThrow();
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(Exception.class).isAssignableFrom(IOException.class).toThrow("msg");
        }, IllegalArgumentException.class, "msg");

        Expect.exception(() -> {
            Assertor.that((Object) null).isAssignableFrom(Exception.class).toThrow("msg");
        }, IllegalArgumentException.class, "msg");

        Expect.exception(() -> {
            Assertor.that(Exception.class).isAssignableFrom(null).toThrow("msg");
        }, IllegalArgumentException.class, "msg");
    }
}