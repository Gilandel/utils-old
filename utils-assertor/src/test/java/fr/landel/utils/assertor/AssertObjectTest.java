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

import java.io.File;
import java.io.IOException;
import java.util.Locale;

import org.junit.Test;

/**
 * Check assertor
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertObjectTest {

    /**
     * Test method for {@link AssertObject#getLocale()}
     * {@link AssertObject#setLocale(Locale)}.
     */
    @Test
    public void testLocale() {
        try {
            assertEquals(Locale.US, Assertor.getLocale());
            Assertor.setLocale(Locale.FRANCE);
            assertEquals(Locale.FRANCE, Assertor.getLocale());
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
            assertEquals("[Assertion failed] ", Assertor.getAssertionPrefix());
            Assertor.setAssertionPrefix("");
            assertEquals("", Assertor.getAssertionPrefix());
            Assertor.setAssertionPrefix("[Assertion failed] ");
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
     * Test method for {@link AssertObject#combine()} .
     */
    @Test
    public void testCombine() {
        AssertCharSequence<String> assertor = Assertor.that("text");

        // intermediate condition (no call of getResult or toThrow), so no reset
        // and this condition is used in the next one
        assertor.contains("__");
        assertFalse(assertor.contains("ext").getResult());
        assertTrue(assertor.contains("__").or().contains("ext").getResult());
        assertTrue(assertor.contains("__").xor().contains("ext").getResult());

        assertFalse(assertor.contains("__").or().contains("ext").and("toto").contains("to").and().contains("r").getResult());
        assertTrue(assertor.contains("__").xor().contains("ext").and("toti").contains("to").and().contains("i").getResult());
        assertFalse(assertor.contains("ext").xor().contains("ext").and("toti").contains("to").and().contains("i").getResult());
        assertFalse(assertor.contains("__").xor().contains("__").and("toti").contains("to").and().contains("i").getResult());

        assertor = Assertor.that("");
        assertor.setCondition(-1);
        assertTrue(assertor.combine(false, "").getResult());
        assertEquals(-1, assertor.getCondition());
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
    }

    /**
     * Test method for {@link AssertObject#isInstanceOf(Class) )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObjectString() {
        Assertor.that(new Exception()).isInstanceOf(IOException.class).toThrow("not instance of");
    }

    /**
     * Test method for {@link AssertObject#isInstanceOf(Class) )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKONull() {
        Assertor.that((Object) null).isInstanceOf(IOException.class).toThrow("not instance of");
    }

    /**
     * Test method for
     * {@link AssertObject#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

        try {
            Assertor.that("texte11").isNotEqual("texte11").toThrow("texte '%2$p' is not equal to '%1$p', %s", "args");
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte11' is not equal to 'texte11', args", e.getMessage());
        }

        try {
            Assertor.that("texte11").isEqual("texte12").toThrow("texte '%2$p' is not equal to '%1$p' or '%p' != '%p'%p...%0$p%3$p");
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte12' is not equal to 'texte11' or 'texte11' != 'texte12'...", e.getMessage());
        }

        try {
            Assertor.that("texte11").isBlank().or().isNotEqual("texte11").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals(
                    "[Assertion failed] this String argument 'texte11' must be null, empty or blank OR Object 'texte11' is equal to Object 'texte11'.",
                    e.getMessage());
        }

        try {
            Assertor.that("texte11").isBlank().or("texte12").isEqual("texte13").toThrow();
            fail("Expect an exception");
        } catch (IllegalArgumentException e) {
            assertEquals(
                    "[Assertion failed] this String argument 'texte11' must be null, empty or blank OR Object 'texte12' is not equal to Object 'texte13'.",
                    e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link AssertObject#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetMessageNullObject() {
        Assertor.that("texte11").isNotEqual("texte11").toThrow("texte '%2$p' is not equal to '%1$p', %s", (Object[]) null);
    }

    /**
     * Test method for {@link AssertObject#not} .
     */
    @Test
    public void testNot() {
        assertTrue(Assertor.that("text").not().isNull().getResult());
        assertTrue(Assertor.that("text").not().isNull().and().isNotNull().getResult());
        assertTrue(Assertor.that("text").not().not().isNotNull().getResult());
        assertFalse(Assertor.that("text").not().isNotNull().getResult());

        Expect.exception(() -> {
            Assertor.that("").not().combine(Assertor.that(""));
            fail();
        }, IllegalArgumentException.class, "'Not' cannot be followed by a condition");
    }

    /**
     * Test method for {@link AssertObject#validate} .
     */
    @Test
    public void testValidate() {
        assertTrue(Assertor.that((Object) 0).validate((Object obj) -> {
            return obj != null;
        }).getResult());

        assertFalse(Assertor.that("/var/log/dev.log").validate((String path) -> {
            if (!new File(path).exists()) {
                throw new IOException();
            }
            return true;
        }).getResult());
    }
}
