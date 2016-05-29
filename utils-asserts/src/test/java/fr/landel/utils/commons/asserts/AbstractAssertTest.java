/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.asserts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Locale;

import org.junit.Test;

import fr.landel.utils.commons.asserts.AssertUtils;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AbstractAssertTest {

    /**
     * Test method for {@link AssertUtils#getLocale()}
     * {@link AssertUtils#setLocale(Locale)}.
     */
    @Test
    public void testLocale() {
        try {
            assertEquals(Locale.US, AssertUtils.getLocale());
            AssertUtils.setLocale(Locale.FRANCE);
            assertEquals(Locale.FRANCE, AssertUtils.getLocale());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#fail(String, String[])}
     * {@link AssertUtils#setLocale(Locale)}.
     * 
     * @throws IOException
     *             The expected exception
     */
    @Test(expected = IOException.class)
    public void testFail2() throws IOException {
        AssertUtils.fail(new IOException(), "error");
    }

    /**
     * Test method for {@link AssertUtils#isFalse(boolean, String, Object...)} .
     */
    @Test
    public void testIsFalseOKBooleanString() {
        try {
            AssertUtils.isFalse(false, "not false");
            AssertUtils.isFalse(false, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isFalse(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBooleanString() {
        AssertUtils.isFalse(true, "not false");
    }

    /**
     * Test method for {@link AssertUtils#isFalse(boolean)} .
     */
    @Test
    public void testIsFalseOKBoolean() {
        try {
            AssertUtils.isFalse(false);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isFalse(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBoolean() {
        AssertUtils.isFalse(true);
    }

    /**
     * Test method for {@link AssertUtils#isTrue(boolean, String, Object...)} .
     */
    @Test
    public void testIsTrueOKBooleanString() {
        try {
            AssertUtils.isTrue(true, "not true");
            AssertUtils.isTrue(true, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isTrue(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBooleanString() {
        AssertUtils.isTrue(false, "not true");
    }

    /**
     * Test method for {@link AssertUtils#isTrue(boolean)} .
     */
    @Test
    public void testIsTrueOKBoolean() {
        try {
            AssertUtils.isTrue(true);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isTrue(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBoolean() {
        AssertUtils.isTrue(false);
    }

    /**
     * Test method for {@link AssertUtils#isNull(Object, String, Object...)} .
     */
    @Test
    public void testIsNullOKObjectString() {
        try {
            AssertUtils.isNull(null, "not null object");
            AssertUtils.isNull(null, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isNull(Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObjectString() {
        AssertUtils.isNull("", "not null object");
    }

    /**
     * Test method for {@link AssertUtils#isNull(java.lang.Object)} .
     */
    @Test
    public void testIsNullOKObject() {
        try {
            AssertUtils.isNull(null);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObject() {
        AssertUtils.isNull("");
    }

    /**
     * Test method for {@link AssertUtils#isNotNull(Object, String, Object...)}
     * .
     */
    @Test
    public void testIsNotNullOKObjectString() {
        try {
            AssertUtils.isNotNull(1, "null object");
            AssertUtils.isNotNull(1, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isNotNull(Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObjectString() {
        AssertUtils.isNotNull(null, "null object");
    }

    /**
     * Test method for {@link AssertUtils#isNotNull(java.lang.Object)} .
     */
    @Test
    public void testIsNotNullOKObject() {
        try {
            AssertUtils.isNotNull(1);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#isNotNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObject() {
        AssertUtils.isNotNull(null);
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test
    public void testIsNotEqualOKObjectObject() {
        try {
            AssertUtils.isNotEqual("texte9", "texte10");
            AssertUtils.isNotEqual("texte9", 5);
            AssertUtils.isNotEqual(5, "texte10");
            AssertUtils.isNotEqual("texte9", null);
            AssertUtils.isNotEqual(null, "texte10");

            StringBuilder sb1 = new StringBuilder("texte9");
            StringBuilder sb2 = new StringBuilder("texte10");
            AssertUtils.isNotEqual(sb1, sb2);
            AssertUtils.isNotEqual(sb1, sb2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObject() {
        AssertUtils.isNotEqual("texte11", "texte11");
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOCharSequence() {
        StringBuilder sb1 = new StringBuilder("texte11");
        StringBuilder sb2 = new StringBuilder("texte11");
        AssertUtils.isNotEqual(sb1, sb2);
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKONull() {
        AssertUtils.isNotEqual((Object) null, null);
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test(expected = IOException.class)
    public void testIsNotEqualKONullException() throws IOException {
        AssertUtils.isNotEqual((Object) null, null, new IOException());
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(Object, Object, String, Object...)} .
     */
    @Test
    public void testIsNotEqualOKObjectObjectString() {
        try {
            AssertUtils.isNotEqual("texte8", "texte7", "equal");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isNotEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObjectString() {
        AssertUtils.isNotEqual("texte6", "texte6", "equal");
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(java.lang.Object, java.lang.Object)} .
     * 
     * @throws IOException
     *             Expected exception if isEqual fails
     */
    @Test
    public void testIsEqualOKObjectObject() throws IOException {
        try {
            AssertUtils.isEqual("texte4", "texte4");
            AssertUtils.isEqual(5, 5);
            AssertUtils.isEqual(5, 5, new IOException());

            StringBuilder sb1 = new StringBuilder("texte4");
            StringBuilder sb2 = new StringBuilder("texte4");
            AssertUtils.isEqual(sb1, sb2);
            AssertUtils.isEqual(sb1, sb2, new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObject() {
        AssertUtils.isEqual("texte5", "texte3");
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(Object, Object, String, Object...)} .
     */
    @Test
    public void testIsEqualOKObjectObjectString() {
        try {
            AssertUtils.isEqual("texte0", "texte0", "not equals");
            AssertUtils.isEqual((Object) null, null, "not equals");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObjectString() {
        AssertUtils.isEqual("texte1", "texte2", "not equals");
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKONullObjectString() {
        AssertUtils.isEqual(null, "texte2", "not equals");
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectNullString() {
        AssertUtils.isEqual("texte1", null, "not equals");
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOIntegerStringString() {
        AssertUtils.isEqual(5, "test", "not equals");
    }

    /**
     * Test method for
     * {@link AssertUtils#isEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOStringIntegerString() {
        AssertUtils.isEqual("test", 5, "not equals");
    }

    /**
     * Test method for
     * {@link AssertUtils#isInstanceOf(java.lang.Class, java.lang.Object)} .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObject() {
        try {
            AssertUtils.isInstanceOf(IOException.class, new IOException());
            AssertUtils.isInstanceOf(IOException.class, new IOException(), new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isInstanceOf(java.lang.Class, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObject() {
        AssertUtils.isInstanceOf(IOException.class, new Exception());
    }

    /**
     * Test method for
     * {@link AssertUtils#isInstanceOf(Class, Object, String, Object...)} .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObjectString() {
        try {
            AssertUtils.isInstanceOf(IOException.class, new IOException(), "not instance of");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isInstanceOf(Class, Object, String, Object...) )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObjectString() {
        AssertUtils.isInstanceOf(IOException.class, new Exception(), "not instance of");
    }

    /**
     * Test method for
     * {@link AssertUtils#isInstanceOf(Class, Object, String, Object...) )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKONull() {
        AssertUtils.isInstanceOf(IOException.class, null, "not instance of");
    }

    /**
     * Test method for
     * {@link AssertUtils#isAssignable(java.lang.Class, java.lang.Class)} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQ() throws IOException {
        try {
            AssertUtils.isAssignable(Exception.class, IOException.class);
            AssertUtils.isAssignable(Exception.class, IOException.class, new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isAssignable(java.lang.Class, java.lang.Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQ() {
        AssertUtils.isAssignable(IOException.class, Exception.class);
    }

    /**
     * Test method for
     * {@link AssertUtils#isAssignable(Class, Class, String, Object...)} .
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQString() {
        try {
            AssertUtils.isAssignable(Exception.class, IOException.class, "msg");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#isAssignable(Class, Class, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQString() {
        AssertUtils.isAssignable(IOException.class, Exception.class, "msg");
    }

    /**
     * Test method for
     * {@link AssertUtils#isAssignable(Class, Class, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOTypeNull() {
        AssertUtils.isAssignable(null, Exception.class, "msg");
    }

    /**
     * Test method for
     * {@link AssertUtils#isAssignable(Class, Class, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOSuperTypeNull() {
        AssertUtils.isAssignable(IOException.class, null, "msg");
    }

    /**
     * Test method for {@link AssertUtils#state(boolean, String, Object...)} .
     */
    @Test
    public void testStateBooleanTrueString() {
        try {
            AssertUtils.state(true, "test");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#state(boolean, String, Object...)} .
     */
    @Test(expected = IllegalStateException.class)
    public void testStateBooleanFalseString() {
        AssertUtils.state(false, "test");
    }

    /**
     * Test method for {@link AssertUtils#state(boolean, String, Object...)} .
     */
    @Test
    public void testStateBooleanFalseString2() {
        try {
            AssertUtils.state(false, "test: %p");
        } catch (IllegalStateException e) {
            assertEquals("[Assertion failed] test: false", e.getMessage());
        }
    }

    /**
     * Test method for {@link AssertUtils#state(boolean)}.
     */
    @Test
    public void testStateBooleanTrue() {
        try {
            AssertUtils.state(true);
            AssertUtils.state(true, new IOException());
        } catch (IOException | IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertUtils#state(boolean)}.
     */
    @Test(expected = IllegalStateException.class)
    public void testStateBooleanFalse() {
        AssertUtils.state(false);
    }

    /**
     * Test method for {@link AssertUtils#state(boolean, Throwable)}.
     * 
     * @throws IOException
     *             Expected exception
     */
    @Test(expected = IOException.class)
    public void testStateBooleanFalseException() throws IOException {
        AssertUtils.state(false, new IOException());
    }

    /**
     * Test method for {@link AssertUtils#fail(java.lang.String)} .
     */
    @Test(expected = IllegalStateException.class)
    public void testFailString() {
        AssertUtils.fail("message");
    }

    /**
     * Test method for {@link AssertUtils#fail(Throwable, String, Object...)} .
     * 
     * @throws IOException
     *             The expected exception
     */
    @Test(expected = IOException.class)
    public void testFailStringThrowable() throws IOException {
        AssertUtils.fail(new IOException(), "message %s", 0);
    }

    /**
     * Test method for {@link AssertUtils#fail(Throwable, String, Object...)} .
     */
    @Test
    public void testFailFormatStringThrowable() {
        IOException exception = null;
        try {
            final double arg0 = 2.25d;
            exception = new IOException();
            AssertUtils.fail(exception, "The argument %s doesn't match the predicate number %.1f", "ARG0", arg0);
            fail("Has to raise an exception");
        } catch (IOException e) {
            assertEquals(exception, e);
            assertEquals(1, e.getSuppressed().length);
            assertEquals(IllegalStateException.class, e.getSuppressed()[0].getClass());
            assertEquals("[Assertion failed] The argument ARG0 doesn't match the predicate number 2.3", e.getSuppressed()[0].getMessage());
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

        try {
            AssertUtils.isNotEqual("texte11", "texte11", "texte '%2$p' is not equal to '%1$p', %s", "args");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte11' is not equal to 'texte11', args", e.getMessage());
        }

        try {
            AssertUtils.isEqual("texte11", "texte12", "texte '%2$p' is not equal to '%1$p' or '%p' != '%p'%p...%0$p%3$p");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte12' is not equal to 'texte11' or 'texte11' != 'texte12'...", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link AssertUtils#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetMessageNullObject() {
        AssertUtils.isNotEqual("texte11", "texte11", "texte '%2$p' is not equal to '%1$p', %s", (Object[]) null);
    }
}
