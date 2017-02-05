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

import org.apache.commons.lang3.tuple.Triple;
import org.junit.Test;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Check {@link TriSupplier}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class TriSupplierTest {

    /**
     * Test method for {@link TriSupplier#get()}.
     */
    @Test
    public void testGet() {
        boolean test = false;
        final String error = "error";

        final TriSupplier<String, String, String> s1 = () -> Triple.of("l", "m", "r");
        final TriSupplier<String, String, String> s2 = () -> {
            if (test) {
                return Triple.of("l", "m", "r");
            }
            throw new IllegalArgumentException(error);
        };

        try {
            assertEquals("(l,m,r)", s1.get().toString());
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
