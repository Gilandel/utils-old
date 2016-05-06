/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;

import org.junit.Test;

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
     * {@link org.gl.utils.commons.Assert#isFalse(boolean, String, Object...)} .
     */
    @Test
    public void testIsFalseOKBooleanString() {
        try {
            Assert.isFalse(false, "not false");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isFalse(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBooleanString() {
        Assert.isFalse(true, "not false");
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#isFalse(boolean)} .
     */
    @Test
    public void testIsFalseOKBoolean() {
        try {
            Assert.isFalse(false);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#isFalse(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBoolean() {
        Assert.isFalse(true);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isTrue(boolean, String, Object...)} .
     */
    @Test
    public void testIsTrueOKBooleanString() {
        try {
            Assert.isTrue(true, "not true");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isTrue(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBooleanString() {
        Assert.isTrue(false, "not true");
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#isTrue(boolean)} .
     */
    @Test
    public void testIsTrueOKBoolean() {
        try {
            Assert.isTrue(true);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#isTrue(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBoolean() {
        Assert.isTrue(false);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNull(Object, String, Object...)} .
     */
    @Test
    public void testIsNullOKObjectString() {
        try {
            Assert.isNull(null, "not null object");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNull(Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObjectString() {
        Assert.isNull("", "not null object");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNull(java.lang.Object)} .
     */
    @Test
    public void testIsNullOKObject() {
        try {
            Assert.isNull(null);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObject() {
        Assert.isNull("");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotNull(Object, String, Object...)}
     * .
     */
    @Test
    public void testIsNotNullOKObjectString() {
        try {
            Assert.isNotNull(1, "null object");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotNull(Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObjectString() {
        Assert.isNotNull(null, "null object");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotNull(java.lang.Object)} .
     */
    @Test
    public void testIsNotNullOKObject() {
        try {
            Assert.isNotNull(1);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObject() {
        Assert.isNotNull(null);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test
    public void testIsNotEqualOKObjectObject() {
        try {
            Assert.isNotEqual("texte9", "texte10");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObject() {
        Assert.isNotEqual("texte11", "texte11");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEqual(Object, Object, String, Object...)}
     * .
     */
    @Test
    public void testIsNotEqualOKObjectObjectString() {
        try {
            Assert.isNotEqual("texte8", "texte7", "equal");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObjectString() {
        Assert.isNotEqual("texte6", "texte6", "equal");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test
    public void testIsEqualOKObjectObject() {
        try {
            Assert.isEqual("texte4", "texte4");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEqual(java.lang.Object, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObject() {
        Assert.isEqual("texte5", "texte3");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test
    public void testIsEqualOKObjectObjectString() {
        try {
            Assert.isEqual("texte0", "texte0", "not equals");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObjectString() {
        Assert.isEqual("texte1", "texte2", "not equals");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isInstanceOf(java.lang.Class, java.lang.Object)}
     * .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObject() {
        try {
            Assert.isInstanceOf(IOException.class, new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isInstanceOf(java.lang.Class, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObject() {
        Assert.isInstanceOf(IOException.class, new Exception());
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isInstanceOf(Class, Object, String, Object...)}
     * .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObjectString() {
        try {
            Assert.isInstanceOf(IOException.class, new IOException(), "not instance of");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isInstanceOf(Class, Object, String, Object...)
     * )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObjectString() {
        Assert.isInstanceOf(IOException.class, new Exception(), "not instance of");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isAssignable(java.lang.Class, java.lang.Class)}
     * .
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQ() {
        try {
            Assert.isAssignable(Exception.class, IOException.class);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isAssignable(java.lang.Class, java.lang.Class)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQ() {
        Assert.isAssignable(IOException.class, Exception.class);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isAssignable(Class, Class, String, Object...)}
     * .
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQString() {
        try {
            Assert.isAssignable(Exception.class, IOException.class, "msg");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isAssignable(Class, Class, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQString() {
        Assert.isAssignable(IOException.class, Exception.class, "msg");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#state(boolean, String, Object...)} .
     */
    @Test
    public void testStateBooleanTrueString() {
        try {
            Assert.state(true, "test");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#state(boolean, String, Object...)} .
     */
    @Test(expected = IllegalStateException.class)
    public void testStateBooleanFalseString() {
        Assert.state(false, "test");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#state(boolean, String, Object...)} .
     */
    @Test
    public void testStateBooleanFalseString2() {
        try {
            Assert.state(false, "test: %p");
        } catch (IllegalStateException e) {
            assertEquals("[Assertion failed] test: false", e.getMessage());
        }
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#state(boolean)}.
     */
    @Test
    public void testStateBooleanTrue() {
        try {
            Assert.state(true);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#state(boolean)}.
     */
    @Test(expected = IllegalStateException.class)
    public void testStateBooleanFalse() {
        Assert.state(false);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#fail(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testFailString() {
        Assert.fail("message");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#fail(Throwable, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testFailStringThrowable() {
        Assert.fail(new IOException(), "message");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#fail(Throwable, String, Object...)} .
     */
    @Test
    public void testFailFormatStringThrowable() {
        try {
            final double arg0 = 2.25d;
            Assert.fail(new IOException(), "The argument %s doesn't match the predicate number %.1f", "ARG0", arg0);
            fail("Has to raise an exception");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] The argument ARG0 doesn't match the predicate number 2.3", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

        try {
            Assert.isNotEqual("texte11", "texte11", "texte '%2$p' is not equal to '%1$p'");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte11' is not equal to 'texte11'", e.getMessage());
        }

        try {
            Assert.isEqual("texte11", "texte12", "texte '%2$p' is not equal to '%1$p' or '%p' != '%p'%p...%0$p%3$p");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte12' is not equal to 'texte11' or 'texte11' != 'texte12'...", e.getMessage());
        }
    }
}