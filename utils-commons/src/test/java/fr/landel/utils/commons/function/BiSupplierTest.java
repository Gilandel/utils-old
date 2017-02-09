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

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Check {@link BiSupplier}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class BiSupplierTest {

    /**
     * Test method for {@link BiSupplier#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final BiSupplier<String, String> s1 = () -> Pair.of("l", "r");
        final BiSupplier<String, String> s2 = () -> {
            if (test) {
                return Pair.of("l", "r");
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals("(l,r)", s1.get().toString());
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
