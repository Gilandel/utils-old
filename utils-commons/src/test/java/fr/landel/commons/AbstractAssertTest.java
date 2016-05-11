/*
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.commons.AssertUtils;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AbstractAssertTest {

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isFalse(boolean, String, Object...)} .
     */
    @Test
    public void testIsFalseOKBooleanString() {
        try {
            AssertUtils.isFalse(false, "not false");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isFalse(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBooleanString() {
        AssertUtils.isFalse(true, "not false");
    }

    /**
     * Test method for {@link fr.landel.utils.commons.AssertUtils#isFalse(boolean)} .
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
     * Test method for {@link fr.landel.utils.commons.AssertUtils#isFalse(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBoolean() {
        AssertUtils.isFalse(true);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isTrue(boolean, String, Object...)} .
     */
    @Test
    public void testIsTrueOKBooleanString() {
        try {
            AssertUtils.isTrue(true, "not true");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isTrue(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBooleanString() {
        AssertUtils.isTrue(false, "not true");
    }

    /**
     * Test method for {@link fr.landel.utils.commons.AssertUtils#isTrue(boolean)} .
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
     * Test method for {@link fr.landel.utils.commons.AssertUtils#isTrue(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBoolean() {
        AssertUtils.isTrue(false);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNull(Object, String, Object...)} .
     */
    @Test
    public void testIsNullOKObjectString() {
        try {
            AssertUtils.isNull(null, "not null object");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNull(Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObjectString() {
        AssertUtils.isNull("", "not null object");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNull(java.lang.Object)} .
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
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObject() {
        AssertUtils.isNull("");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotNull(Object, String, Object...)}
     * .
     */
    @Test
    public void testIsNotNullOKObjectString() {
        try {
            AssertUtils.isNotNull(1, "null object");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotNull(Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObjectString() {
        AssertUtils.isNotNull(null, "null object");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotNull(java.lang.Object)} .
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
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObject() {
        AssertUtils.isNotNull(null);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test
    public void testIsNotEqualOKObjectObject() {
        try {
            AssertUtils.isNotEqual("texte9", "texte10");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObject() {
        AssertUtils.isNotEqual("texte11", "texte11");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isNotEqual(Object, Object, String, Object...)}
     * .
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
     * {@link fr.landel.utils.commons.AssertUtils#isNotEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObjectString() {
        AssertUtils.isNotEqual("texte6", "texte6", "equal");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test
    public void testIsEqualOKObjectObject() {
        try {
            AssertUtils.isEqual("texte4", "texte4");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObject() {
        AssertUtils.isEqual("texte5", "texte3");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test
    public void testIsEqualOKObjectObjectString() {
        try {
            AssertUtils.isEqual("texte0", "texte0", "not equals");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObjectString() {
        AssertUtils.isEqual("texte1", "texte2", "not equals");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isInstanceOf(java.lang.Class, java.lang.Object)}
     * .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObject() {
        try {
            AssertUtils.isInstanceOf(IOException.class, new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isInstanceOf(java.lang.Class, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObject() {
        AssertUtils.isInstanceOf(IOException.class, new Exception());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isInstanceOf(Class, Object, String, Object...)}
     * .
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
     * {@link fr.landel.utils.commons.AssertUtils#isInstanceOf(Class, Object, String, Object...)
     * )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObjectString() {
        AssertUtils.isInstanceOf(IOException.class, new Exception(), "not instance of");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isAssignable(java.lang.Class, java.lang.Class)}
     * .
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQ() {
        try {
            AssertUtils.isAssignable(Exception.class, IOException.class);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isAssignable(java.lang.Class, java.lang.Class)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQ() {
        AssertUtils.isAssignable(IOException.class, Exception.class);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#isAssignable(Class, Class, String, Object...)}
     * .
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
     * {@link fr.landel.utils.commons.AssertUtils#isAssignable(Class, Class, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQString() {
        AssertUtils.isAssignable(IOException.class, Exception.class, "msg");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#state(boolean, String, Object...)} .
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
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#state(boolean, String, Object...)} .
     */
    @Test(expected = IllegalStateException.class)
    public void testStateBooleanFalseString() {
        AssertUtils.state(false, "test");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#state(boolean, String, Object...)} .
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
     * Test method for {@link fr.landel.utils.commons.AssertUtils#state(boolean)}.
     */
    @Test
    public void testStateBooleanTrue() {
        try {
            AssertUtils.state(true);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link fr.landel.utils.commons.AssertUtils#state(boolean)}.
     */
    @Test(expected = IllegalStateException.class)
    public void testStateBooleanFalse() {
        AssertUtils.state(false);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#fail(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testFailString() {
        AssertUtils.fail("message");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#fail(Throwable, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testFailStringThrowable() {
        AssertUtils.fail(new IOException(), "message");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#fail(Throwable, String, Object...)} .
     */
    @Test
    public void testFailFormatStringThrowable() {
        try {
            final double arg0 = 2.25d;
            AssertUtils.fail(new IOException(), "The argument %s doesn't match the predicate number %.1f", "ARG0", arg0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] The argument ARG0 doesn't match the predicate number 2.3", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.AssertUtils#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

        try {
            AssertUtils.isNotEqual("texte11", "texte11", "texte '%2$p' is not equal to '%1$p'");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte11' is not equal to 'texte11'", e.getMessage());
        }

        try {
            AssertUtils.isEqual("texte11", "texte12", "texte '%2$p' is not equal to '%1$p' or '%p' != '%p'%p...%0$p%3$p");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte12' is not equal to 'texte11' or 'texte11' != 'texte12'...", e.getMessage());
        }
    }
}
