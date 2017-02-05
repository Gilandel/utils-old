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

import org.junit.Test;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Check {@link BooleanSupplierThrowable}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class BooleanSupplierThrowableTest {

    /**
     * Test method for {@link BooleanSupplierThrowable#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final BooleanSupplierThrowable<IllegalArgumentException> s1 = () -> true;
        final BooleanSupplierThrowable<IllegalArgumentException> s2 = () -> {
            if (test) {
                return true;
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals(true, s1.getAsBoolean());
        } catch (FunctionException e) {
            fail("Supplier failed");
        }

        try {
            s2.getAsBoolean();
            fail("Supplier has to fail");
        } catch (FunctionException e) {
            assertNotNull(e);
            assertEquals("java.lang.IllegalArgumentException: " + error, e.getMessage());
        }
    }

    /**
     * Test method for {@link BooleanSupplierThrowable#getThrows()}.
     */
    @Test
    public void testGetThrows() {
        boolean test = false;
        final String error = "error";

        final BooleanSupplierThrowable<IllegalArgumentException> s1 = () -> true;
        final BooleanSupplierThrowable<IllegalArgumentException> s2 = () -> {
            if (test) {
                return true;
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals(true, s1.getAsBooleanThrows());
        } catch (IllegalArgumentException e) {
            fail("Supplier failed");
        }

        try {
            s2.getAsBooleanThrows();
            fail("Supplier has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error, e.getMessage());
        }
    }
}
