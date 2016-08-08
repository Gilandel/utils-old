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
import static org.junit.Assert.fail;

import java.util.Locale;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check {@link AssertorHelper}
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public class AssertorHelperTest extends AbstractTest {

    /**
     * Test method for {@link AssertorHelper#getMessage} .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

        assertEquals("default", AssertorHelper.getMessage("default", null, null, null, null));
        assertEquals("message", AssertorHelper.getMessage("default", null, "message", null, null));
        assertEquals("message test", AssertorHelper.getMessage("default", null, "message %s", null, new Object[] {"test"}));
        assertEquals("message 23.26", AssertorHelper.getMessage("default", Locale.US, "message %.2f", null, new Object[] {23.256f}));
        assertEquals("message 23,26", AssertorHelper.getMessage("default", Locale.FRANCE, "message %.2f", null, new Object[] {23.256f}));

        Expect.exception(() -> {
            Assertor.that(23.6f).isEqual(25.6f).toThrow();
            fail();
        }, IllegalArgumentException.class, "the number '23,600' should be equal to '25,600'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("texte11").not().isEqual("texte11").toThrow("texte '%2$s*' is not equal to '%1$s*', %s", "args");
            fail();
        }, IllegalArgumentException.class, "texte 'texte11' is not equal to 'texte11', args");

        Expect.exception(() -> {
            Assertor.that("texte11").isEqual(true).toThrow("texte '%2$s*' is not equal to '%1$s*', %s", "args");
            fail();
        }, IllegalArgumentException.class, "texte 'true' is not equal to 'texte11', args");

        try {
            Assertor.that("texte11").isNotEqual("texte11").toThrow("texte '%2$s*' is not equal to '%1$s*', %s", "args");
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("texte 'texte11' is not equal to 'texte11', args", e.getMessage());
        }

        try {
            Assertor.that("texte11").isEqual("texte12").toThrow("texte '%2$s*' is not equal to '%1$s*' or '%s*' != '%s*'...");
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("texte 'texte12' is not equal to 'texte11' or 'texte11' != 'texte12'...", e.getMessage());
        }

        Expect.exception(() -> {
            Assertor.that("texte11").isBlank().or().isNotEqual("texte11").toThrow();
            fail("Expect an exception");
        }, IllegalArgumentException.class,
                "the char sequence 'texte11' should be null, empty or blank OR the object 'texte11' should NOT be equal to 'texte11'",
                JUNIT_ERROR);

        try {
            Assertor.that("texte11").isNotBlank().and().isNotEqual("texte11").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("the object 'texte11' should NOT be equal to 'texte11'", e.getMessage());
        }

        try {
            Assertor.that("texte11").isBlank().or().not().isEqual("texte11").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals(
                    "the char sequence 'texte11' should be null, empty or blank OR the object 'texte11' should NOT be equal to 'texte11'",
                    e.getMessage());
        }

        try {
            Assertor.that("texte11").isBlank().or("texte12").isEqual("texte13").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("the char sequence 'texte11' should be null, empty or blank OR the object 'texte12' should be equal to 'texte12'",
                    e.getMessage());
        }

        Expect.exception(() -> {
            Assertor.that("texte11").isBlank().or("texte12").not().startsWith("text").or().isBlank().toThrow();
            fail("Expect an exception");
        }, IllegalArgumentException.class,
                "the char sequence 'texte11' should be null, empty or blank"
                        + " OR the char sequence 'texte12' should NOT start with 'text'"
                        + " OR the char sequence 'texte12' should be null, empty or blank",
                JUNIT_ERROR);

        // prerequisites == false
        Expect.exception(() -> {
            Assertor.that("texte11").isBlank().or("texte12").not().startsWith("text").or().isBlank().toThrow();
            fail("Expect an exception");
        }, IllegalArgumentException.class,
                "the char sequence 'texte11' should be null, empty or blank"
                        + " OR the char sequence 'texte12' should NOT start with 'text'"
                        + " OR the char sequence 'texte12' should be null, empty or blank",
                JUNIT_ERROR);

        // previous assertion is invalid (prerequisites == false), only first
        // prerequisite error set as message
        assertEquals("the char sequence cannot be null and the searched substring cannot be null or empty",
                Assertor.that("text1").contains(null).and("text2").isBlank().getErrors());
    }

    /**
     * Test method for {@link AssertorHelper#getMessage} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetMessageNullObject() {
        Assertor.that("texte11").isNotEqual("texte11").toThrow("texte '%2$s*' is not equal to '%1$s*', %s", (Object[]) null);
    }

    /**
     * Test method for {@link AssertorConstants#getProperty} .
     */
    @Test
    public void testGetProperty() {
        assertEquals(DEFAULT_ASSERTION, AssertorConstants.getProperty(null, ""));
        assertEquals("the boolean should be true", AssertorConstants.getProperty("boolean.true"));
        assertEquals("the boolean should be true", AssertorConstants.getProperty("boolean.true", "arg"));
        assertEquals("the object '{0}' should be null", AssertorConstants.getProperty("object.null"));
        assertEquals("the object 'arg' should be null", AssertorConstants.getProperty("object.null", "arg"));
        assertEquals("the object 'arg' should be null", AssertorConstants.getProperty("object.null", "arg", ""));
    }
}
