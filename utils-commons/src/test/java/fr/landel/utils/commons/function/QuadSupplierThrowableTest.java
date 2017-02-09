/*-
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
package fr.landel.utils.commons.function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.commons.exception.FunctionException;
import fr.landel.utils.commons.tuple.Quad;

/**
 * Check {@link QuadSupplierThrowable}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class QuadSupplierThrowableTest {

    /**
     * Test method for {@link QuadSupplierThrowable#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final QuadSupplierThrowable<String, String, String, String, IllegalArgumentException> s1 = () -> Quad.of("a", "b", "c", "d");
        final QuadSupplierThrowable<String, String, String, String, IOException> s2 = () -> {
            if (test) {
                return Quad.of("a", "b", "c", "d");
            }
            throw new IOException(error);
        };

        try {
            assertEquals("(a,b,c,d)", s1.get().toString());
        } catch (FunctionException e) {
            fail("Supplier failed");
        }

        try {
            s2.get();
            fail("Supplier has to fail");
        } catch (FunctionException e) {
            assertNotNull(e);
            assertEquals("java.io.IOException: " + error, e.getMessage());
        }
    }

    /**
     * Test method for {@link QuadSupplierThrowable#getThrows()}.
     */
    @Test
    public void testGetThrows() {
        boolean test = false;
        final String error = "error";

        final QuadSupplierThrowable<String, String, String, String, IllegalArgumentException> s1 = () -> Quad.of("a", "b", "c", "d");
        final QuadSupplierThrowable<String, String, String, String, IOException> s2 = () -> {
            if (test) {
                return Quad.of("a", "b", "c", "d");
            }
            throw new IOException(error);
        };

        try {
            assertEquals("(a,b,c,d)", s1.getThrows().toString());
        } catch (IllegalArgumentException e) {
            fail("Supplier failed");
        }

        try {
            s2.getThrows();
            fail("Supplier has to fail");
        } catch (IOException e) {
            assertNotNull(e);
            assertEquals(error, e.getMessage());
        }
    }
}
