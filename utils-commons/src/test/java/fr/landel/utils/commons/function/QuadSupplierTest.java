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
import fr.landel.utils.commons.tuple.Quad;

/**
 * Check {@link QuadSupplier}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class QuadSupplierTest {

    /**
     * Test method for {@link QuadSupplier#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final QuadSupplier<String, String, String, String> s1 = () -> Quad.of("a", "b", "c", "d");
        final QuadSupplier<String, String, String, String> s2 = () -> {
            if (test) {
                return Quad.of("a", "b", "c", "d");
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals("(a,b,c,d)", s1.get().toString());
        } catch (FunctionException e) {
            fail("Supplier failed");
        }

        try {
            s2.get();
            fail("Supplier has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error, e.getMessage());
        }
    }
}
