/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.stream;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check {@link SupplierThrowable}
 *
 * @since 30 mai 2016
 * @author Gilles
 *
 */
public class SupplierThrowableTest {

    /**
     * Test method for {@link SupplierThrowable#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final SupplierThrowable<String, IllegalArgumentException> s1 = () -> "";
        final SupplierThrowable<String, IllegalArgumentException> s2 = () -> {
            if (test) {
                return "";
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals("", s1.get());
        } catch (RuntimeException e) {
            fail("Supplier failed");
        }

        try {
            s2.get();
            fail("Supplier has to fail");
        } catch (RuntimeException e) {
            assertNotNull(e);
            assertEquals("java.lang.IllegalArgumentException: " + error, e.getMessage());
        }
    }

    /**
     * Test method for {@link SupplierThrowable#getThrows()}.
     */
    @Test
    public void testGetThrows() {
        boolean test = false;
        final String error = "error";

        final SupplierThrowable<String, IllegalArgumentException> s1 = () -> "";
        final SupplierThrowable<String, IllegalArgumentException> s2 = () -> {
            if (test) {
                return "";
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals("", s1.getThrows());
        } catch (IllegalArgumentException e) {
            fail("Supplier failed");
        }

        try {
            s2.getThrows();
            fail("Supplier has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error, e.getMessage());
        }
    }
}
