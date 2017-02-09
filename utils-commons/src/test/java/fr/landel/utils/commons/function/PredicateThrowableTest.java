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
 * Check {@link PredicateThrowable}
 *
 * @since May 30, 2016
 * @author Gilles
 *
 */
public class PredicateThrowableTest {

    private static final String ERROR1 = "The argument is null";
    private static final String ERROR2 = "String is not in upper case";

    private static final PredicateThrowable<String, IllegalArgumentException> P1 = (s1) -> {
        if (s1 != null) {
            return s1.length() > 2;
        }
        throw new IllegalArgumentException(ERROR1);
    };

    private static final PredicateThrowable<String, IllegalArgumentException> P2 = (s1) -> {
        String s1u = s1.toUpperCase();
        if (s1u.equals(s1)) {
            return s1u.contains("V");
        }
        throw new IllegalArgumentException(ERROR2);
    };

    /**
     * Test method for {@link PredicateThrowable#test(java.lang.Object)}.
     */
    @Test
    public void testTest() {
        try {
            assertTrue(P1.test("v12"));
        } catch (FunctionException e) {
            fail("Predicate failed");
        }

        try {
            P1.test(null);
            fail("Predicate has to fail");
        } catch (FunctionException e) {
            assertNotNull(e);
            assertEquals("java.lang.IllegalArgumentException: " + ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for {@link PredicateThrowable#testThrows(java.lang.Object)}.
     */
    @Test
    public void testTestThrows() {
        try {
            assertTrue(P1.testThrows("v12"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            P1.testThrows(null);
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for {@link PredicateThrowable#and(PredicateThrowable)}.
     */
    @Test
    public void testAnd() {
        final PredicateThrowable<String, IllegalArgumentException> pp = P1.and(P2);

        try {
            assertTrue(pp.testThrows("V12"));
            assertFalse(pp.testThrows("V1"));
            assertFalse(pp.testThrows("A69"));
            assertFalse(pp.testThrows("A6"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            pp.testThrows(null);
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            pp.testThrows("v12");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR2, e.getMessage());
        }
    }

    /**
     * Test method for {@link PredicateThrowable#negateThrows()}.
     */
    @Test
    public void testNegateThrows() {
        final PredicateThrowable<String, IllegalArgumentException> pp = P1.negateThrows();

        try {
            assertFalse(pp.testThrows("V12"));
            assertTrue(pp.testThrows("v6"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            pp.testThrows(null);
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }
    }

    /**
     * Test method for {@link PredicateThrowable#or(PredicateThrowable)}.
     */
    @Test
    public void testOr() {
        final PredicateThrowable<String, IllegalArgumentException> pp = P1.or(P2);

        try {
            assertTrue(pp.testThrows("V1"));
            assertTrue(pp.testThrows("V12"));
            assertTrue(pp.testThrows("v12"));
            assertFalse(pp.testThrows("A1"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            pp.testThrows(null);
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR1, e.getMessage());
        }

        try {
            // first test pass and return true, so the second one is not
            // executed
            assertTrue(pp.testThrows("v12"));
        } catch (IllegalArgumentException e) {
            fail("Predicate failed");
        }

        try {
            // first test pass and return false, so the next is executed
            pp.testThrows("v6");
            fail("Predicate has to fail");
        } catch (IllegalArgumentException e) {
            assertNotNull(e);
            assertEquals(ERROR2, e.getMessage());
        }
    }
}
