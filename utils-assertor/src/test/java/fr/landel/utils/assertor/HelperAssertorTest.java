/*-

 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check {@link HelperAssertor}
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public class HelperAssertorTest extends AbstractTest {

    /**
     * Test method for {@link HelperAssertor#HelperAssertor()} .
     */
    @Test
    public void testConstructor() {
        assertNotNull(new HelperAssertor());
    }

    /**
     * Test method for {@link HelperAssertor#combine} .
     */
    @Test
    public void testCombine() {
        final AssertorResult<String> a = new AssertorResult<>("test", EnumType.CHAR_SEQUENCE);
        final AssertorResult<Boolean> b = new AssertorResult<>(true, EnumType.BOOLEAN);

        // precondition: true & true, valid: true & true
        AssertorResult<String> assertorResult1 = new AssertorResult<>(a, true, true, "pre-error1", "error1");
        AssertorResult<Boolean> assertorResult2 = new AssertorResult<>(b, true, true, "pre-error2", "error2");

        AssertorResult<String> assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        AssertorResult<String> assertorResult3 = HelperAssertor.combine(assertorResult, (result) -> true, null,
                (objectIndex, paramSize) -> "pre-error3", (objectIndex, paramSize, not) -> "error3", false);

        assertTrue(assertorResult3.isPreconditionOK());
        assertTrue(assertorResult3.isValid());

        // NOT

        assertorResult3 = HelperAssertor.combine(new AssertorResult<>(assertorResult), (result) -> true, null,
                (objectIndex, paramSize) -> "pre-error3", (objectIndex, paramSize, not) -> "error3", false);

        assertTrue(assertorResult3.isPreconditionOK());
        assertFalse(assertorResult3.isValid());

        // ALL PRECONDITIONS KO

        assertorResult1 = new AssertorResult<>(a, false, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, true, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        assertorResult3 = HelperAssertor.combine(assertorResult, (result) -> false, null, (objectIndex, paramSize) -> "pre-error3",
                (objectIndex, paramSize, not) -> "error3", false);

        assertFalse(assertorResult3.isPreconditionOK());
        assertEquals("pre-error1 AND pre-error3", assertorResult3.getPreconditionMessage().toString());

        // MESSAGE NULL

        assertorResult1 = new AssertorResult<>(a, true, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, true, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        assertorResult3 = HelperAssertor.combine(assertorResult, (result) -> true, (result, not) -> false,
                (objectIndex, paramSize) -> "pre-error3", null, false);

        assertTrue(assertorResult3.isPreconditionOK());
        assertFalse(assertorResult3.isValid());
        assertEquals("", assertorResult3.getMessage().toString());

        // OPERATOR NULL (== AND)

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, null);

        assertorResult3 = HelperAssertor.combine(assertorResult, (result) -> true, (result, not) -> false,
                (objectIndex, paramSize) -> "pre-error3", (objectIndex, paramSize, not) -> "error3", false);

        assertTrue(assertorResult3.isPreconditionOK());
        assertFalse(assertorResult3.isValid());
        assertEquals("error3", assertorResult3.getMessage().toString());
    }
}
