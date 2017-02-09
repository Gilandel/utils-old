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
 * Check {@link FunctionThrowable}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class ConsumerThrowableTest {

    private static final String ERROR = "The parameter is null";
    private static final ConsumerThrowable<String, IllegalArgumentException> C = (s1) -> {
        if (s1 == null) {
            throw new IllegalArgumentException(ERROR);
        }
    };

    /**
     * Test method for {@link FunctionThrowable#accept(java.lang.Object)}.
     */
    @Test
    public void testAccept() {
        try {
            C.accept("v1");
        } catch (FunctionException e) {
            fail("Consumer failed");
        }

        try {
            C.accept(null);
            fail("Consumer has to fail");
        } catch (FunctionException e) {
            assertNotNull(e);
            assertEquals("java.lang.IllegalArgumentException: " + ERROR, e.getMessage());
        }
    }

    /**
     * Test method for {@link FunctionThrowable#acceptThrows(java.lang.Object)}.
     */
    @Test
    public void testAcceptThrows() {
        try {
            C.acceptThrows("v1");
        } catch (IllegalArgumentException e) {
            fail("Consumer failed");
        }

        try {
            C.acceptThrows(null);
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR, e.getMessage());
        }
    }

    /**
     * Test method for {@link ConsumerThrowable#andThen(ConsumerThrowable)}.
     */
    @Test
    public void testAndThen() {
        try {
            C.andThen(null);
            fail("Consumer has to fail");
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        final String error1 = "The parameter is null";
        final String error2 = "The parameter is empty";

        final ConsumerThrowable<String, IllegalArgumentException> c2 = C.andThen((s1) -> {
            if (s1.length() == 0) {
                throw new IllegalArgumentException(error2);
            }
        });

        try {
            c2.acceptThrows("v1");
        } catch (IllegalArgumentException e) {
            fail("Consumer failed");
        }

        try {
            c2.acceptThrows(null);
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error1, e.getMessage());
        }

        try {
            c2.acceptThrows("");
            fail("Consumer has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(error2, e.getMessage());
        }
    }
}
