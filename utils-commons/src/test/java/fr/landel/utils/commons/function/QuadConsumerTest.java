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

/**
 * Check {@link QuadConsumer}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class QuadConsumerTest {

    private static final String ERROR = "At least one parameter is null";
    private static final QuadConsumer<String, String, String, String> C = (s1, s2, s3, s4) -> {
        if (s1 == null || s2 == null || s3 == null || s4 == null) {
            throw new IllegalArgumentException(ERROR);
        }
    };

    /**
     * Test method for
     * {@link QuadConsumer#accept(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testAccept() {
        try {
            C.accept("v1", "v2", "v3", "v4");
        } catch (IllegalArgumentException e) {
            fail("Consumer failed");
        }

        try {
            C.accept("v1", null, "v3", "v4");
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR, e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link QuadConsumer#acceptThrows(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testAcceptThrows() {
        try {
            C.accept("v1", "v2", "v3", "v4");
        } catch (IllegalArgumentException e) {
            fail("Consumer failed");
        }

        try {
            C.accept("v1", null, "v3", "v4");
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link QuadConsumer#andThen(QuadConsumer)}.
     */
    @Test
    public void testAndThen() {
        try {
            C.andThen(null);
            fail("Consumer has to fail");
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        final String error1 = "First parameter is null";
        final String error2 = "Second parameter is null";
        final String error3 = "Evil error";

        final QuadConsumer<String, String, String, String> c1 = (s1, s2, s3, s4) -> {
            if (s1 == null) {
                throw new IllegalArgumentException(error1);
            }
        };

        final QuadConsumer<String, String, String, String> c2 = c1.andThen((s1, s2, s3, s4) -> {
            if (s2 == null) {
                throw new IllegalArgumentException(error2);
            }
        }).andThen((s1, s2, s3, s4) -> {
            throw new IllegalArgumentException(error3);
        });

        try {
            c2.accept(null, "v2", "v3", "v4");
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error1, e.getMessage());
        }

        try {
            c2.accept("v1", null, "v3", "v4");
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error2, e.getMessage());
        }

        try {
            c2.accept("v1", "v2", "v3", "v4");
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error3, e.getMessage());
        }
    }
}
