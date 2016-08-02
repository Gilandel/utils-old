/*
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
import static org.junit.Assert.fail;

import java.awt.Color;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Locale;

import org.junit.Test;

import fr.landel.utils.assertor.Constants.TYPE;
import fr.landel.utils.assertor.expect.Expect;

/**
 * Check assertor
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertObjectTest extends AbstractTest {

    /**
     * Test method for {@link AssertObject#getLocale()}
     * {@link AssertObject#setLocale(Locale)}.
     */
    @Test
    public void testLocale() {
        try {
            Assertor.setLocale(Locale.US);
            assertEquals(Locale.US, Assertor.getLocale());
            assertEquals("Test 3.14", Assertor.that(1).isNotEqual(1, "Test %.2f", Math.PI).getErrors());
            assertEquals("Test 3,14", Assertor.that(1).isNotEqual(1, Locale.FRANCE, "Test %.2f", Math.PI).getErrors());

            Assertor.setLocale(Locale.FRANCE);
            assertEquals(Locale.FRANCE, Assertor.getLocale());
            assertEquals("Test 3,14", Assertor.that(1).isNotEqual(1, "Test %.2f", Math.PI).getErrors());
            assertEquals("Test 3.14", Assertor.that(1).isNotEqual(1, Locale.US, "Test %.2f", Math.PI).getErrors());

            // Reset
            Assertor.setLocale(Locale.getDefault());
            assertEquals(Locale.getDefault(), Assertor.getLocale());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#getAssertionPrefix()}
     * {@link AssertObject#setAssertionPrefix(String)}.
     */
    @Test
    public void testAssertionPrefix() {
        try {
            assertEquals("", Assertor.getAssertionPrefix());
            Assertor.setAssertionPrefix("[");
            assertEquals("[", Assertor.getAssertionPrefix());
            assertEquals("[Test", Assertor.that(1).isNotEqual(1, "Test").getErrors());

            Assertor.setAssertionPrefix("");
            assertEquals("Test", Assertor.that(1).isNotEqual(1, "Test").getErrors());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#getAssertionSuffix()}
     * {@link AssertObject#setAssertionSuffix(String)}.
     */
    @Test
    public void testAssertionSuffix() {
        try {
            assertEquals("", Assertor.getAssertionSuffix());
            Assertor.setAssertionSuffix("]");
            assertEquals("]", Assertor.getAssertionSuffix());
            assertEquals("Test]", Assertor.that(1).isNotEqual(1, "Test").getErrors());

            Assertor.setAssertionSuffix("");
            assertEquals("Test", Assertor.that(1).isNotEqual(1, "Test").getErrors());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#getObjectClass()}.
     */
    @Test
    public void testGetObjectClasses() {
        assertNotNull(Assertor.that(new Object()).getObjectClass());
    }

    /**
     * Test method for {@link AssertObject#combine()}.
     */
    @Test
    public void testCombine() {
        AssertCharSequence<String> assertor = Assertor.that("text");

        // intermediate condition (no call of isOK or toThrow), so no reset
        // and this condition is used in the next one
        assertor.contains("__");
        assertFalse(assertor.contains("ext").isOK());
        assertTrue(assertor.contains("__").or().contains("ext").isOK());
        assertTrue(assertor.contains("__").xor().contains("ext").isOK());

        assertFalse(assertor.contains("__").or().contains("ext").and("toto").contains("to").and().contains("r").isOK());
        assertTrue(assertor.contains("__").xor().contains("ext").and("toti").contains("to").and().contains("i").isOK());
        assertFalse(assertor.contains("ext").xor().contains("ext").and("toti").contains("to").and().contains("i").isOK());
        assertFalse(assertor.contains("__").xor().contains("__").and("toti").contains("to").and().contains("i").isOK());

        assertEquals("text1 text2",
                Assertor.that(1).combine(true, () -> false, null, "%s %2$s*", new Object[] {"text1"}, Locale.FRANCE, "text2").getErrors());
        assertEquals("text2", Assertor.that(1).combine(true, () -> false, null, "%2$s*", null, Locale.FRANCE, "text2").getErrors());
        assertEquals("text1 text2",
                Assertor.that(1).combine(true, () -> false, null, "%s %2$s*", new Object[] {"text1"}, null, "text2").getErrors());
        assertEquals(Constants.DEFAULT_ASSERTION,
                Assertor.that(1).combine(true, () -> false, null, null, new Object[] {"text1"}, null, "text2").getErrors());
        assertEquals(Constants.DEFAULT_ASSERTION,
                Assertor.that(1).combine(true, () -> false, null, null, new Object[] {"text1"}, Locale.FRANCE, "text2").getErrors());

        assertor = Assertor.that("");
        assertor.setCondition(-1);
        assertTrue(assertor.combine(true, () -> false, null, "").isOK());
        assertEquals(0, assertor.getCondition()); // cleared
        assertFalse(assertor.combine(true, () -> false, null, "").isOK());

        // basic tests (XOR / NOT) + prerequisites == true
        assertTrue(assertor.combine(true, null, null, "").isOK());
        assertTrue(assertor.combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(true, () -> false, null, "").isOK());

        assertFalse(assertor.not().combine(true, null, null, "").isOK());
        assertFalse(assertor.not().combine(true, () -> true, null, "").isOK());
        assertTrue(assertor.not().combine(true, () -> false, null, "").isOK());

        // prerequisites == false
        assertFalse(assertor.combine(false, null, null, "").isOK());
        assertFalse(assertor.not().combine(false, null, null, "").isOK());
        assertFalse(assertor.not().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.not().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.not().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.not().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, () -> false, null, "").isOK());

        // previous prerequisites == false
        assertFalse(assertor.combine(false, null, null, "").and().combine(true, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().combine(true, () -> false, null, "").isOK());

        assertFalse(assertor.combine(false, null, null, "").or().combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").xor().combine(true, () -> true, null, "").isOK());

        assertFalse(assertor.combine(false, null, null, "").and().not().combine(true, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(true, () -> false, null, "").isOK());

        assertFalse(assertor.combine(false, null, null, "").and().combine(false, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(false, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().not().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and().combine(false, () -> false, null, "").isOK());

        // previous assertor with prerequisites == false
        assertFalse(assertor.combine(false, null, null, "").and("").combine(true, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").combine(true, () -> false, null, "").isOK());

        assertFalse(assertor.combine(false, null, null, "").or("").combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").xor("").combine(true, () -> true, null, "").isOK());

        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(true, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(true, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(true, () -> false, null, "").isOK());

        assertFalse(assertor.combine(false, null, null, "").and("").combine(false, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(false, null, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").not().combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").combine(false, () -> true, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").combine(false, () -> false, null, "").isOK());
        assertFalse(assertor.combine(false, null, null, "").and("").combine(false, () -> false, null, "").isOK());
    }

    /**
     * Test method for {@link AssertObject#isNull()} .
     */
    @Test
    public void testIsNullOKObjectString() {
        try {
            Assertor.that((Object) null).isNull().toThrow("not null object");
            Assertor.that((Object) null).isNull().toThrow(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isNull()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObjectString() {
        Assertor.that("").isNull().toThrow("not null object");
    }

    /**
     * Test method for {@link AssertObject#isNull()} .
     */
    @Test
    public void testIsNullOKObject() {
        try {
            Assertor.that((Object) null).isNull().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isNull()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObject() {
        Assertor.that("").isNull().toThrow();
    }

    /**
     * Test method for {@link AssertObject#isNotNull()} .
     */
    @Test
    public void testIsNotNullOKObjectString() {
        try {
            Assertor.that(1).isNotNull().toThrow("null object");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isNotNull()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObjectString() {
        Assertor.that((Object) null).isNotNull().toThrow("null object");
    }

    /**
     * Test method for {@link AssertObject#isNotNull()} .
     */
    @Test
    public void testIsNotNullOKObject() {
        try {
            Assertor.that(1).isNotNull();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isNotNull()} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObject() {
        Assertor.that((Object) null).isNotNull().toThrow(new IllegalArgumentException());
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     */
    @Test
    public void testIsNotEqualOKObjectObject() {
        try {
            Assertor.that("texte9").isNotEqual("texte10").and().isNotEqual(5).toThrow();
            Assertor.that(5).isNotEqual("texte10").toThrow();
            Assertor.that("texte9").isNotEqual(null).toThrow();
            Assertor.that((String) null).isNotEqual("texte10").toThrow();

            StringBuilder sb1 = new StringBuilder("texte9");
            StringBuilder sb2 = new StringBuilder("texte10");
            Assertor.that(sb1).isNotEqual(sb2).toThrow(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObject() {
        Assertor.that("texte11").isNotEqual("texte11").toThrow();
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOCharSequence() {
        StringBuilder sb1 = new StringBuilder("texte11");
        StringBuilder sb2 = new StringBuilder("texte11");
        Assertor.that(sb1).isNotEqual(sb2).toThrow();
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKONull() {
        Assertor.that((Object) null).isNotEqual(null).toThrow();
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test(expected = IOException.class)
    public void testIsNotEqualKONullException() throws IOException {
        Assertor.that((Object) null).isNotEqual(null).toThrow(new IOException());
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     */
    @Test
    public void testIsNotEqualOKObjectObjectString() {
        try {
            Assertor.that("texte8").isNotEqual("texte7").toThrow("equal");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isNotEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObjectString() {
        Assertor.that("texte6").isNotEqual("texte6").toThrow("equal");
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     * 
     * @throws IOException
     *             exception if isEqual fails
     */
    @Test
    public void testIsEqualOKObjectObject() throws IOException {
        try {
            Assertor.that("texte4").isEqual("texte4");
            Assertor.that(5).isEqual(5).toThrow(new IOException());

            StringBuilder sb1 = new StringBuilder("texte4");
            StringBuilder sb2 = new StringBuilder("texte4");
            Assertor.that(sb1).isEqual(sb2).toThrow(new IOException());

            Assertor.that(Color.BLACK).isEqual(new Color(0)).toThrow(new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObject() {
        Assertor.that("texte5").isEqual("texte3").toThrow();
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test
    public void testIsEqualOKObjectObjectString() {
        try {
            Assertor.that("texte0").isEqual("texte0").toThrow("not equals");
            Assertor.that((Object) null).isEqual(null).toThrow("not equals");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObjectString() {
        Assertor.that("texte1").isEqual("texte2").toThrow("not equals");
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKONullObjectString() {
        Assertor.that((Object) null).isEqual("texte2").toThrow("not equals");
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectNullString() {
        Assertor.that("texte1").isEqual(null).toThrow("not equals");
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOIntegerStringString() {
        Assertor.that(5).isEqual("test").toThrow("not equals");
    }

    /**
     * Test method for {@link AssertObject#isEqual(Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOStringIntegerString() {
        Assertor.that("test").isEqual(5).toThrow("not equals");
    }

    /**
     * Test method for {@link AssertObject#isInstanceOf(Class)} .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObject() {
        try {
            Assertor.that(new IOException()).isInstanceOf(IOException.class).toThrow(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertObject#isInstanceOf(Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObject() {
        Assertor.that(new Exception()).isInstanceOf(IOException.class).toThrow();
    }

    /**
     * Test method for {@link AssertObject#isInstanceOf(Class)} .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObjectString() {
        try {
            Assertor.that(new IOException()).isInstanceOf(IOException.class).toThrow("not instance of");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }

        Expect.exception(() -> {
            Assertor.that(new Exception()).isInstanceOf(IOException.class).toThrow("not instance of");
            fail();
        }, IllegalArgumentException.class, "not instance of");

        Expect.exception(() -> {
            Assertor.that((Object) null).isInstanceOf(IOException.class).toThrow("not instance of");
            fail();
        }, IllegalArgumentException.class, "not instance of");

        Expect.exception(() -> {
            Assertor.that(new Exception()).isInstanceOf(null).toThrow("not instance of");
            fail();
        }, IllegalArgumentException.class, "not instance of");
    }

    /**
     * Test method for {@link AssertObject#isAssignableFrom} .
     */
    @Test
    public void testIsAssignableFrom() {
        assertTrue(Assertor.that(Color.BLACK).isAssignableFrom(Color.class).isOK());
        assertFalse(Assertor.that(Color.BLACK).isAssignableFrom(Point.class).isOK());
        assertFalse(Assertor.that((Color) null).isAssignableFrom(Color.class).isOK());
        assertFalse(Assertor.that(Color.BLACK).isAssignableFrom(null).isOK());

        Expect.exception(() -> {
            Assertor.that((Object) null).isAssignableFrom(Exception.class).toThrow("msg");
        }, IllegalArgumentException.class, "msg");

    }

    /**
     * Test method for
     * {@link AssertObject#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

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
                "the char sequence 'texte11' should be null, empty or blank OR the object 'texte11' should be NOT equal to 'texte11'");

        try {
            Assertor.that("texte11").isNotBlank().and().isNotEqual("texte11").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("the object 'texte11' should be NOT equal to 'texte11'", e.getMessage());
        }

        try {
            Assertor.that("texte11").isBlank().or().not().isEqual("texte11").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals(
                    "the char sequence 'texte11' should be null, empty or blank OR NOT (the object 'texte11' should be equal to 'texte11')",
                    e.getMessage());
        }

        try {
            Assertor.that("texte11").isBlank().or("texte12").isEqual("texte13").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals(
                    "(the char sequence 'texte11' should be null, empty or blank) OR (the object 'texte12' should be equal to 'texte12')",
                    e.getMessage());
        }

        Expect.exception(() -> {
            Assertor.that("texte11").isBlank().or("texte12").not().startsWith("text").or().isBlank().toThrow();
            fail("Expect an exception");
        }, IllegalArgumentException.class,
                "(the char sequence 'texte11' should be null, empty or blank) OR (NOT (the char sequence 'texte12' should start with 'text') OR the char sequence 'texte12' should be null, empty or blank)",
                JUNIT_ERROR);

        // prerequisites == false
        Expect.exception(() -> {
            Assertor.that("texte11").isBlank().or("texte12").not().startsWith("text").or().isBlank().toThrow();
            fail("Expect an exception");
        }, IllegalArgumentException.class,
                "(the char sequence 'texte11' should be null, empty or blank) OR (NOT (the char sequence 'texte12' should start with 'text') OR the char sequence 'texte12' should be null, empty or blank)",
                JUNIT_ERROR);

        // previous assertion is invalid (prerequisites == false), only first
        // prerequisite error set as message
        assertEquals("the char sequence cannot be null and the searched substring cannot be null or empty",
                Assertor.that("text1").contains(null).and("text2").isBlank().getErrors());
    }

    /**
     * Test method for
     * {@link AssertObject#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetMessageNullObject() {
        Assertor.that("texte11").isNotEqual("texte11").toThrow("texte '%2$s*' is not equal to '%1$s*', %s", (Object[]) null);
    }

    /**
     * Test method for {@link AssertObject#not} .
     */
    @Test
    public void testNot() {
        assertTrue(Assertor.that("text").not().isNull().isOK());
        assertTrue(Assertor.that("text").not().isNull().and().isNotNull().isOK());
        assertTrue(Assertor.that("text").not().not().isNotNull().isOK());
        assertFalse(Assertor.that("text").not().isNotNull().isOK());

        Expect.exception(() -> {
            Assertor.that("").not().combine(Constants.AND, Assertor.that(""));
            fail();
        }, IllegalArgumentException.class, "'Not' cannot be followed by a condition");
    }

    /**
     * Test method for {@link AssertObject#validates} .
     */
    @Test
    public void testValidates() {
        assertTrue(Assertor.that((Object) 0).validates((obj) -> {
            return obj != null;
        }).isOK());

        assertFalse(Assertor.that("/var/log/dev.log").validates((path) -> {
            if (!new File(path).exists()) {
                throw new IOException();
            }
            return true;
        }).isOK());

        assertFalse(Assertor.that("/var/log/dev.log").validates(null).isOK());

        assertTrue(Assertor.that((Object) null).validates((obj) -> {
            return obj == null;
        }, "Path is invalid").isOK());

        assertTrue(Assertor.that((Object) 0).validates((obj) -> {
            return obj != null;
        }, "Path is invalid").isOK());

        assertFalse(Assertor.that("/var/log/dev.log").validates((path) -> {
            if (!new File(path).exists()) {
                throw new IOException();
            }
            return true;
        }, "Path '%1$s*' provide by '%s' is invalid", "John").isOK());

        assertTrue(Assertor.that((Object) 0).validates((obj) -> {
            return obj != null;
        }, Locale.US, "Path is invalid").isOK());

        assertEquals("Path '/var/log/dev.log' provided by 'John' is invalid in '10.27'ms",
                Assertor.that("/var/log/dev.log").validates((path) -> {
                    if (!new File(path).exists()) {
                        throw new IOException();
                    }
                    return true;
                }, Locale.US, "Path '%1$s*' provided by '%s' is invalid in '%.2f'ms", "John", 10.26589f).getErrors());

        assertEquals("Path '/var/log/dev.log' provided by 'John' is invalid in '10,27'ms",
                Assertor.that("/var/log/dev.log").validates((path) -> {
                    if (!new File(path).exists()) {
                        throw new IOException();
                    }
                    return true;
                }, Locale.FRANCE, "Path '%1$s*' provided by '%s' is invalid in '%.2f'ms", "John", 10.26589f).getErrors());
    }

    /**
     * Test method for {@link AssertObject#getType} .
     */
    @Test
    public void testGetType() {
        assertEquals(TYPE.BOOLEAN, AssertObject.getType(true));
        assertEquals(TYPE.BOOLEAN, AssertObject.getType(Boolean.FALSE));

        assertEquals(TYPE.NUMBER_INTEGER, AssertObject.getType((byte) 1));
        assertEquals(TYPE.NUMBER_INTEGER, AssertObject.getType((short) 1));
        assertEquals(TYPE.NUMBER_INTEGER, AssertObject.getType(1));
        assertEquals(TYPE.NUMBER_INTEGER, AssertObject.getType(1L));
        assertEquals(TYPE.NUMBER_INTEGER, AssertObject.getType(new BigInteger("12")));

        assertEquals(TYPE.NUMBER_DECIMAL, AssertObject.getType(3.25f));
        assertEquals(TYPE.NUMBER_DECIMAL, AssertObject.getType(3.25d));
        assertEquals(TYPE.NUMBER_DECIMAL, AssertObject.getType(new BigDecimal("12.25")));

        assertEquals(TYPE.ARRAY, AssertObject.getType(new Object[0]));

        assertEquals(TYPE.ITERABLE, AssertObject.getType(Collections.EMPTY_LIST));
        assertEquals(TYPE.ITERABLE, AssertObject.getType(Collections.EMPTY_SET));

        assertEquals(TYPE.MAP, AssertObject.getType(Collections.EMPTY_MAP));

        assertEquals(TYPE.DATE, AssertObject.getType(new Date()));
        assertEquals(TYPE.DATE, AssertObject.getType(Calendar.getInstance()));

        assertEquals(TYPE.CHAR_SEQUENCE, AssertObject.getType(""));
        assertEquals(TYPE.CHAR_SEQUENCE, AssertObject.getType(new StringBuilder()));

        assertEquals(TYPE.CLASS, AssertObject.getType(new Date().getClass()));

        assertEquals(TYPE.UNKNOWN, AssertObject.getType(Color.BLACK));
        assertEquals(TYPE.UNKNOWN, AssertObject.getType(null));
    }
}
