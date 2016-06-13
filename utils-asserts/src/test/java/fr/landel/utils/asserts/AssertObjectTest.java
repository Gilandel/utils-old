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
package fr.landel.utils.asserts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Locale;

import org.junit.Test;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertObjectTest {

    /**
     * Test method for {@link Expect#getLocale()}
     * {@link Expect#setLocale(Locale)}.
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
     * Test method for {@link Expect#isFalse(boolean, String, Object...)} .
     */
    @Test
    public void testIsFalseOKBooleanString() {
        try {
            AssertUtils.check(false).isFalse("not false");
            AssertUtils.check(false).isFalse(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isFalse(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBooleanString() {
        AssertUtils.check(true).isFalse("not false");
    }

    /**
     * Test method for {@link Expect#isFalse(boolean)} .
     */
    @Test
    public void testIsFalseOKBoolean() {
        try {
            AssertUtils.check(false).isFalse();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isFalse(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsFalseKOBoolean() {
        AssertUtils.check(true).isFalse();
    }

    /**
     * Test method for {@link Expect#isTrue(boolean, String, Object...)} .
     */
    @Test
    public void testIsTrueOKBooleanString() {
        try {
            AssertUtils.check(true).isTrue("not true").isTrue(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isTrue(boolean, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBooleanString() {
        AssertUtils.check(false).isTrue("not true");
    }

    /**
     * Test method for {@link Expect#isTrue(boolean)} .
     */
    @Test
    public void testIsTrueOKBoolean() {
        try {
            AssertUtils.check(true).isTrue();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isTrue(boolean)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsTrueKOBoolean() {
        AssertUtils.check(false).isTrue();
    }

    /**
     * Test method for {@link Expect#isNull(Object, String, Object...)} .
     */
    @Test
    public void testIsNullOKObjectString() {
        try {
            AssertUtils.check((Object) null).isNull("not null object");
            AssertUtils.check((Object) null).isNull(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNull(Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObjectString() {
        AssertUtils.check("").isNull("not null object");
    }

    /**
     * Test method for {@link Expect#isNull(java.lang.Object)} .
     */
    @Test
    public void testIsNullOKObject() {
        try {
            AssertUtils.check((Object) null).isNull();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNullKOObject() {
        AssertUtils.check("").isNull();
    }

    /**
     * Test method for {@link Expect#isNotNull(Object, String, Object...)} .
     */
    @Test
    public void testIsNotNullOKObjectString() {
        try {
            AssertUtils.check(1).isNotNull("null object").isNotNull(new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotNull(Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObjectString() {
        AssertUtils.check((Object) null).isNotNull("null object");
    }

    /**
     * Test method for {@link Expect#isNotNull(java.lang.Object)} .
     */
    @Test
    public void testIsNotNullOKObject() {
        try {
            AssertUtils.check(1).isNotNull();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotNull(java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotNullKOObject() {
        AssertUtils.check((Object) null).isNotNull();
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test
    public void testIsNotEqualOKObjectObject() {
        try {
            AssertUtils.check("texte9").isNotEqual("texte10").isNotEqual(5);
            AssertUtils.check(5).isNotEqual("texte10");
            AssertUtils.check("texte9").isNotEqual(null);
            AssertUtils.check((String) null).isNotEqual("texte10");

            StringBuilder sb1 = new StringBuilder("texte9");
            StringBuilder sb2 = new StringBuilder("texte10");
            AssertUtils.check(sb1).isNotEqual(sb2).isNotEqual(sb2, new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObject() {
        AssertUtils.check("texte11").isNotEqual("texte11");
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOCharSequence() {
        StringBuilder sb1 = new StringBuilder("texte11");
        StringBuilder sb2 = new StringBuilder("texte11");
        AssertUtils.check(sb1).isNotEqual(sb2);
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKONull() {
        AssertUtils.check((Object) null).isNotEqual(null);
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(java.lang.Object, java.lang.Object)} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test(expected = IOException.class)
    public void testIsNotEqualKONullException() throws IOException {
        AssertUtils.check((Object) null).isNotEqual(null, new IOException());
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(Object, Object, String, Object...)} .
     */
    @Test
    public void testIsNotEqualOKObjectObjectString() {
        try {
            AssertUtils.check("texte8").isNotEqual("texte7", "equal");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isNotEqual(Object, Object, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEqualKOObjectObjectString() {
        AssertUtils.check("texte6").isNotEqual("texte6", "equal");
    }

    /**
     * Test method for
     * {@link Expect#isEqual(java.lang.Object, java.lang.Object)} .
     * 
     * @throws IOException
     *             Expected exception if isEqual fails
     */
    @Test
    public void testIsEqualOKObjectObject() throws IOException {
        try {
            AssertUtils.check("texte4").isEqual("texte4");
            AssertUtils.check(5).isEqual(5).isEqual(5, new IOException());

            StringBuilder sb1 = new StringBuilder("texte4");
            StringBuilder sb2 = new StringBuilder("texte4");
            AssertUtils.check(sb1).isEqual(sb2).isEqual(sb2, new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isEqual(java.lang.Object, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObject() {
        AssertUtils.check("texte5").isEqual("texte3");
    }

    /**
     * Test method for {@link Expect#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test
    public void testIsEqualOKObjectObjectString() {
        try {
            AssertUtils.check("texte0").isEqual("texte0", "not equals");
            AssertUtils.check((Object) null).isEqual(null, "not equals");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectObjectString() {
        AssertUtils.check("texte1").isEqual("texte2", "not equals");
    }

    /**
     * Test method for {@link Expect#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKONullObjectString() {
        AssertUtils.check((Object) null).isEqual("texte2", "not equals");
    }

    /**
     * Test method for {@link Expect#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOObjectNullString() {
        AssertUtils.check("texte1").isEqual(null, "not equals");
    }

    /**
     * Test method for {@link Expect#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOIntegerStringString() {
        AssertUtils.check(5).isEqual("test", "not equals");
    }

    /**
     * Test method for {@link Expect#isEqual(Object, Object, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEqualKOStringIntegerString() {
        AssertUtils.check("test").isEqual(5, "not equals");
    }

    /**
     * Test method for
     * {@link Expect#isInstanceOf(java.lang.Class, java.lang.Object)} .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObject() {
        try {
            AssertUtils.check(new IOException()).isInstanceOf(IOException.class).isInstanceOf(IOException.class,
                    new IllegalArgumentException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isInstanceOf(java.lang.Class, java.lang.Object)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObject() {
        AssertUtils.check(new Exception()).isInstanceOf(IOException.class);
    }

    /**
     * Test method for
     * {@link Expect#isInstanceOf(Class, Object, String, Object...)} .
     */
    @Test
    public void testIsInstanceOfOKClassOfQObjectString() {
        try {
            AssertUtils.check(new IOException()).isInstanceOf(IOException.class, "not instance of");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isInstanceOf(Class, Object, String, Object...) )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKOClassOfQObjectString() {
        AssertUtils.check(new Exception()).isInstanceOf(IOException.class, "not instance of");
    }

    /**
     * Test method for
     * {@link Expect#isInstanceOf(Class, Object, String, Object...) )} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsInstanceOfKONull() {
        AssertUtils.check((Object) null).isInstanceOf(IOException.class, "not instance of");
    }

    /**
     * Test method for
     * {@link Expect#isAssignable(java.lang.Class, java.lang.Class)} .
     * 
     * @throws IOException
     *             On errors
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQ() throws IOException {
        try {
            AssertUtils.check(IOException.class).isAssignable(Exception.class).isAssignable(Exception.class, new IOException());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isAssignable(java.lang.Class, java.lang.Class)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQ() {
        AssertUtils.check(Exception.class).isAssignable(IOException.class);
    }

    /**
     * Test method for
     * {@link Expect#isAssignable(Class, Class, String, Object...)} .
     */
    @Test
    public void testIsAssignableOKClassOfQClassOfQString() {
        try {
            AssertUtils.check(IOException.class).isAssignable(Exception.class, "msg");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isAssignable(Class, Class, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOClassOfQClassOfQString() {
        AssertUtils.check(Exception.class).isAssignable(IOException.class, "msg");
    }

    /**
     * Test method for
     * {@link Expect#isAssignable(Class, Class, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOTypeNull() {
        AssertUtils.check((Object) null).isAssignable(Exception.class, "msg");
    }

    /**
     * Test method for
     * {@link Expect#isAssignable(Class, Class, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsAssignableKOSuperTypeNull() {
        AssertUtils.check((Object) null).isAssignable(IOException.class, "msg");
    }

    /**
     * Test method for
     * {@link Expect#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test
    public void testGetMessage() {
        // TEST GET MESSAGE

        try {
            AssertUtils.check("texte11").isNotEqual("texte11", "texte '%2$p' is not equal to '%1$p', %s", "args");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte11' is not equal to 'texte11', args", e.getMessage());
        }

        try {
            AssertUtils.check("texte11").isEqual("texte12", "texte '%2$p' is not equal to '%1$p' or '%p' != '%p'%p...%0$p%3$p");
        } catch (IllegalArgumentException e) {
            assertEquals("[Assertion failed] texte 'texte12' is not equal to 'texte11' or 'texte11' != 'texte12'...", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link Expect#getMessage(java.lang.String, java.lang.String, java.lang.Object[], java.lang.Object[])}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetMessageNullObject() {
        AssertUtils.check("texte11").isNotEqual("texte11", "texte '%2$p' is not equal to '%1$p', %s", (Object[]) null);
    }
}
