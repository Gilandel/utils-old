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
package fr.landel.utils.commons.function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Check {@link BiSupplierThrowable}
 *
 * @since 30 mai 2016
 * @author Gilles
 *
 */
public class BiSupplierThrowableTest {

    /**
     * Test method for {@link BiSupplierThrowable#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final BiSupplierThrowable<String, String, IllegalArgumentException> s1 = () -> Pair.of("l", "r");
        final BiSupplierThrowable<String, String, IOException> s2 = () -> {
            if (test) {
                return Pair.of("l", "r");
            }
            throw new IOException(error);
        };

        try {
            assertEquals("(l,r)", s1.get().toString());
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
     * Test method for {@link BiSupplierThrowable#getThrows()}.
     */
    @Test
    public void testGetThrows() {
        boolean test = false;
        final String error = "error";

        final BiSupplierThrowable<String, String, IllegalArgumentException> s1 = () -> Pair.of("l", "r");
        final BiSupplierThrowable<String, String, IOException> s2 = () -> {
            if (test) {
                return Pair.of("l", "r");
            }
            throw new IOException(error);
        };

        try {
            assertEquals("(l,r)", s1.getThrows().toString());
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
