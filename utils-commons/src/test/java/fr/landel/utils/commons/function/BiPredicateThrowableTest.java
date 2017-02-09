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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import fr.landel.utils.commons.exception.FunctionException;

/**
 * Check {@link BiPredicateThrowable}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class BiPredicateThrowableTest {

    private static final String ERROR1 = "The first argument is null";
    private static final String ERROR2 = "The second argument is null";
    private static final String ERROR3 = "Both arguments are null";
    private static final String ERROR4 = "First string is not in upper case";
    private static final String ERROR5 = "Second string is not in upper case";
    private static final String ERROR6 = "Both strings are not in upper case";

    private static final BiPredicateThrowable<String, String, IllegalArgumentException> P1 = (s1, s2) -> {
        if (s1 != null && s2 != null) {
            return s1.length() > s2.length();
        } else if (s1 != null) {
            throw new IllegalArgumentException(ERROR2);
        } else if (s2 != null) {
            throw new IllegalArgumentException(ERROR1);
        }
        throw new IllegalArgumentException(ERROR3);
    };

    private static final BiPredicateThrowable<String, String, IllegalArgumentException> P2 = (s1, s2) -> {
        String s1u = s1.toUpperCase();
        String s2u = s2.toUpperCase();
        if (s1u.equals(s1) && s2u.equals(s2)) {
            return s1u.contains(s2u) || s2u.contains(s1u);
        } else if (!s1u.equals(s1)) {
            throw new IllegalArgumentException(ERROR4);
        } else if (!s2u.equals(s2)) {
            throw new IllegalArgumentException(ERROR5);
        }
        throw new IllegalArgumentException(ERROR6);
    };

    /**
     * Test method for
     * {@link BiPredicateThrowable#test(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testTest() {
        try {
            assertTrue(P1.test("v12", "v8"));
        } catch (FunctionException e) {
            fail("Predicate failed");
        }

        try {
            P1.test(null, "v2");
            fail("Predicate has to fail");
        } catch (FunctionException e) {
            assertNotNull(e);
            assertEquals("java.lang.IllegalArgumentException: " + ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link BiPredicateThrowable#testThrows(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testTestThrows() {
        try {
            assertTrue(P1.testThrows("v12", "v8"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            P1.testThrows(null, "v2");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for {@link BiPredicateThrowable#and(BiPredicateThrowable)}.
     */
    @Test
    public void testAnd() {
        final BiPredicateThrowable<String, String, IllegalArgumentException> pp = P1.and(P2);

        try {
            assertTrue(pp.testThrows("V12", "V1"));
            assertFalse(pp.testThrows("V12", "V8"));
            assertFalse(pp.testThrows("V6", "V12"));
            assertFalse(pp.testThrows("V6", "V6"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            pp.testThrows(null, "V8");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            pp.testThrows("V12", "v8");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR5, e.getMessage());
        }
    }

    /**
     * Test method for {@link BiPredicateThrowable#negateThrows()}.
     */
    @Test
    public void testNegateThrows() {
        final BiPredicateThrowable<String, String, IllegalArgumentException> pp = P1.negateThrows();

        try {
            assertFalse(pp.testThrows("V12", "V8"));
            assertTrue(pp.testThrows("v6", "V8"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            pp.testThrows("V6", null);
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR2, e.getMessage());
        }
    }

    /**
     * Test method for {@link BiPredicateThrowable#or(BiPredicateThrowable)}.
     */
    @Test
    public void testOr() {
        final BiPredicateThrowable<String, String, IllegalArgumentException> pp = P1.or(P2);

        try {
            assertTrue(pp.testThrows("V12", "V1"));
            assertTrue(pp.testThrows("V", "V1"));
            assertTrue(pp.testThrows("V12", "V12"));
            assertFalse(pp.testThrows("V6", "V12"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            pp.testThrows(null, "V8");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            // first test pass and return true, so the second one is not
            // executed
            assertTrue(pp.testThrows("V12", "v8"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            // first test pass and return false, so the next is executed
            pp.testThrows("v6", "V8");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR4, e.getMessage());
        }
    }
}
