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
public class FunctionThrowableTest {

    private static final String ERROR1 = "The first argument is null or one character";
    private static final String ERROR2 = "Value has to be equal to zero";

    private static final FunctionThrowable<String, Integer, IllegalArgumentException> FN1 = (s1) -> {
        if (s1 != null && s1.length() > 0) {
            return s1.length() - 1;
        }
        throw new IllegalArgumentException(ERROR1);
    };

    private static final FunctionThrowable<Integer, Integer, IllegalArgumentException> FN2 = (u) -> {
        if (u == 1) {
            return u + 10;
        }
        throw new IllegalArgumentException(ERROR2);
    };

    private static final FunctionThrowable<String, Integer, IllegalArgumentException> FN3 = FN2.composeThrows(FN1);
    private static final FunctionThrowable<String, Integer, IllegalArgumentException> FN4 = FN1.andThen(FN2);

    /**
     * Test method for {@link FunctionThrowable#apply(java.lang.Object)}.
     */
    @Test
    public void testApply() {
        try {
            assertEquals(1, FN1.apply("v1").intValue());
        } catch (FunctionException e) {
            fail("Function failed");
        }

        try {
            FN1.apply(null);
            fail("Function has to fail");
        } catch (FunctionException e) {
            assertNotNull(e);
            assertEquals("java.lang.IllegalArgumentException: " + ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for {@link FunctionThrowable#applyThrows(java.lang.Object)}.
     */
    @Test
    public void testApplyThrows() {
        try {
            assertEquals(1, FN1.applyThrows("v1").intValue());
        } catch (IllegalArgumentException e) {
            fail("Function failed");
        }

        try {
            FN1.applyThrows(null);
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link FunctionThrowable#composeThrows(FunctionThrowable)}.
     */
    @Test
    public void testComposeThrows() {
        try {
            FN2.composeThrows(null);
            fail("Function has to fail");
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            FN3.applyThrows(null);
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            FN3.applyThrows("");
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            FN3.applyThrows("v");
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR2, e.getMessage());
        }

        try {
            assertEquals(11, FN3.applyThrows("v1").intValue());
        } catch (IllegalArgumentException e) {
            fail("Function failed");
        }
    }

    /**
     * Test method for {@link FunctionThrowable#andThen(FunctionThrowable)}.
     */
    @Test
    public void testAndThen() {
        try {
            FN1.andThen(null);
            fail("Function has to fail");
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            FN4.applyThrows(null);
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            FN4.applyThrows("");
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            FN4.applyThrows("1");
            fail("Function has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR2, e.getMessage());
        }

        try {
            assertEquals(11, FN4.applyThrows("v1").intValue());
        } catch (IllegalArgumentException e) {
            fail("Function failed");
        }
    }
}
