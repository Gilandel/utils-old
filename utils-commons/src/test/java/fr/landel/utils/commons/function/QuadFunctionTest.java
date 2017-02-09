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
 * Check {@link QuadFunction}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class QuadFunctionTest {

    private static final QuadFunction<String, String, String, String, Integer> FN1 = (s1, s2, s3, s4) -> {
        if (s1 != null && s2 != null) {
            return s1.length() + s2.length();
        } else if (s1 != null) {
            return -1;
        } else if (s2 != null) {
            return -2;
        }
        return -3;
    };

    private static final QuadFunction<String, String, String, String, Integer> FN2 = FN1.andThen((u) -> {
        if (u == 0) {
            return u + 10;
        }
        return -1;
    });

    /**
     * Test method for
     * {@link QuadFunction#apply(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testApply() {
        assertEquals(4, FN1.apply("v1", "v2", "v3", "v4").intValue());
        assertEquals(-2, FN1.apply(null, "v2", "v3", "v4").intValue());
    }

    /**
     * Test method for
     * {@link QuadFunction#applyThrows(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testApplyThrows() {
        assertEquals(4, FN1.apply("v1", "v2", "v3", "v4").intValue());
        assertEquals(-2, FN1.apply(null, "v2", "v3", "v4").intValue());
    }

    /**
     * Test method for {@link QuadFunction#andThen(FunctionThrowable)}.
     */
    @Test
    public void testAndThen() {
        try {
            FN1.andThen(null);
            fail("Function has to fail");
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        assertEquals(-1, FN2.apply("", null, "v3", "v4").intValue());
        assertEquals(-1, FN2.apply("v", "", "v3", "v4").intValue());
        assertEquals(10, FN2.apply("", "", "v3", "v4").intValue());
    }
}
