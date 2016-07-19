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

import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.assertor.AssertClass;
import fr.landel.utils.assertor.Assertor;

/**
 * Check assert class
 *
 * @since 18 juil. 2016
 * @author Gilles
 *
 */
public class AssertClassTest {

    /**
     * Test method for {@link AssertClass#isAssignableFrom(Class)} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test
    public void testisAssignableFromOKClassOfQClassOfQ() throws IOException {
        try {
            Assertor.that(IOException.class).isAssignableFrom(Exception.class).toThrow(new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertClass#isAssignableFrom(Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testisAssignableFromKOClassOfQClassOfQ() {
        Assertor.that(Exception.class).isAssignableFrom(IOException.class).toThrow();
    }

    /**
     * Test method for {@link AssertClass#isAssignableFrom(Class)} .
     */
    @Test
    public void testisAssignableFromOKClassOfQClassOfQString() {
        try {
            Assertor.that(IOException.class).isAssignableFrom(Exception.class).toThrow("msg");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertClass#isAssignableFrom(Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testisAssignableFromKOClassOfQClassOfQString() {
        Assertor.that(Exception.class).isAssignableFrom(IOException.class).toThrow("msg");
    }

    /**
     * Test method for {@link AssertClass#isAssignableFrom(Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testisAssignableFromKOTypeNull() {
        Assertor.that((Object) null).isAssignableFrom(Exception.class).toThrow("msg");
    }

    /**
     * Test method for {@link AssertClass#isAssignableFrom(Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testisAssignableFromKOSuperTypeNull() {
        Assertor.that((Object) null).isAssignableFrom(IOException.class).toThrow("msg");
    }

}
