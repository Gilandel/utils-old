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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Objects;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check {@link AssertorBoolean}
 *
 * @since Jul 18, 2016
 * @author Gilles
 *
 */
public class AssertorBooleanTest extends AbstractTest {

    /**
     * Test method for {@link AssertorBoolean#AssertorBoolean()} .
     */
    @Test
    public void testConstructor() {
        assertNotNull(new AssertorBoolean());
    }

    /**
     * Test method for {@link AssertorBoolean} .
     */
    @Test
    public void testPredicateGet() {
        assertFalse(Assertor.that(true).hasHashCode(0).isOK());
        assertTrue(Assertor.that(true).hasHashCode(Objects.hashCode(true)).isOK());
    }

    /**
     * Test method for {@link AssertorBoolean#isFalse()} .
     */
    @Test
    public void testIsFalse() {
        StepAssertor<Boolean> assertorResult = new StepAssertor<>(true, EnumType.BOOLEAN);
        assertFalse(AssertorBoolean.isFalse(assertorResult, null).getChecker().test(true, false));

        assertTrue(Assertor.that(true).isFalse().or(Assertor.that("").isEmpty()).isOK());

        try {
            Assertor.that(false).isFalse().toThrow("not false");
            Assertor.that(false).isFalse().and(true).not().isFalse().toThrow("not false");
            Assertor.that(false).isFalse().toThrow(new IllegalArgumentException(), true);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(true).isFalse().toThrow("not false");
            fail();
        }, IllegalArgumentException.class, "not false");

        Expect.exception(() -> {
            Assertor.that(true).isFalse("%s, '%s*'", "not false").toThrow();
            fail();
        }, IllegalArgumentException.class, "not false, 'true'");

        Expect.exception(() -> {
            Assertor.that(true).isFalse().toThrow(new IOException("not false"), true);
            fail();
        }, IOException.class, "not false");
    }

    /**
     * Test method for {@link AssertorBoolean#isTrue()} .
     */
    @Test
    public void testIsTrue() {
        try {
            Assertor.that(true).isTrue().toThrow();
            Assertor.that(true).isTrue().and(false).not().isTrue().toThrow("not true");
            Assertor.that(true).isTrue().toThrow(new IllegalArgumentException(), true);

            Assertor.that(true).isTrue().and().not().isFalse().toThrow();
            Assertor.that(true).isTrue().or().isFalse().toThrow();
            Assertor.that(true).isTrue().xor().isFalse().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(false).isTrue().toThrow("not true");
            fail();
        }, IllegalArgumentException.class, "not true", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(false).isTrue("%s, '%s*'", "not true").toThrow();
            fail();
        }, IllegalArgumentException.class, "not true, 'false'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that(false).isTrue().toThrow(new IOException("not true"), true);
            fail();
        }, IOException.class, "not true", JUNIT_ERROR);
    }
}