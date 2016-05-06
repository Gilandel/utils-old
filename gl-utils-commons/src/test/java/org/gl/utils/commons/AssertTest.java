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

import java.awt.Color;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hamcrest.Matcher;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertTest {

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
     * {@link org.gl.utils.commons.Assert#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKStringString() {
        try {
            Assert.isNotEmpty("a", "empty string");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOStringString() {
        Assert.isNotEmpty("", "empty string");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2StringString() {
        Assert.isNotEmpty((String) null, "empty string");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.lang.String)} .
     */
    @Test
    public void testIsNotEmptyOKString() {
        try {
            Assert.isNotEmpty("z");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOString() {
        Assert.isNotEmpty("");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2String() {
        Assert.isNotEmpty((String) null);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(Object[], String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKObjectArrayString() {
        try {
            Assert.isNotEmpty(Arrays.asList("").toArray(), "empty array");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(Object[], String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArrayString() {
        Assert.isNotEmpty(Collections.emptyList().toArray(), "empty array");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.lang.Object[])} .
     */
    @Test
    public void testIsNotEmptyOKObjectArray() {
        try {
            Assert.isNotEmpty(Arrays.asList("").toArray());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.lang.Object[])} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArray() {
        Assert.isNotEmpty(Collections.emptyList().toArray());
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQString() {
        try {
            Assert.isNotEmpty(Arrays.asList(""), "empty collection");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQString() {
        Assert.isNotEmpty(Collections.emptyList(), "empty collection");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.util.Collection)} .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQ() {
        try {
            Assert.isNotEmpty(Arrays.asList(""));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.util.Collection)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQ() {
        Assert.isNotEmpty(Collections.emptyList());
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(Map, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQString() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("f", "f");
            Assert.isNotEmpty(map, "empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(Map, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQString() {
        Assert.isNotEmpty(new HashMap<String, String>(), "empty");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.util.Map)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQ() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("fg", "fg");
            Assert.isNotEmpty(map);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotEmpty(java.util.Map)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQ() {
        Assert.isNotEmpty(new HashMap<String, String>());
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEmpty(String, String, Object...)} .
     */
    @Test
    public void testIsEmptyOKStringString() {
        try {
            Assert.isEmpty(null, "not empty or null");
            Assert.isEmpty("", "not empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOStringString() {
        Assert.isEmpty("r", "not empty");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEmpty(java.lang.String)} .
     */
    @Test
    public void testIsEmptyOKString() {
        try {
            Assert.isEmpty(null);
            Assert.isEmpty("");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOString() {
        Assert.isEmpty("e");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotBlank(String, String, Object...)}
     * .
     */
    @Test
    public void testIsNotBlankOKStringString() {
        try {
            Assert.isNotBlank("   \t sds  ", "blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotBlank(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOStringString() {
        Assert.isNotBlank("   \t    ", "blank");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotBlank(java.lang.String)} .
     */
    @Test
    public void testIsNotBlankOKString() {
        try {
            Assert.isNotBlank("    \t  e ");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isNotBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOString() {
        Assert.isNotBlank("    \t   ");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isBlank(String, String, Object...)} .
     */
    @Test
    public void testIsBlankOKStringString() {
        try {
            Assert.isBlank("   \t   ", "not blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOStringString() {
        Assert.isBlank("   \t d   ", "not blank");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isBlank(java.lang.String)} .
     */
    @Test
    public void testIsBlankOKString() {
        try {
            Assert.isBlank("   \t   ");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#isBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOString() {
        Assert.isBlank("      j ");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test
    public void testDoesNotContainOKStringStringString() {
        try {
            Assert.doesNotContain("titi part en vacances", "toto", "not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringStringString() {
        Assert.doesNotContain("titi part en vacances", "titi", "not found");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testDoesNotContainOKStringString() {
        try {
            Assert.doesNotContain("toto part en vacances", "tutu");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringString() {
        Assert.doesNotContain("tata part en vacances", "tata");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testContainsOKStringString() {
        try {
            Assert.contains("toto part en vacances", "toto");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringString() {
        Assert.contains("tata part en vacances", "tutu");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#contains(String, String, String, Object...)}
     * .
     */
    @Test
    public void testContainsOKStringStringString() {
        try {
            Assert.contains("toto part en vacances", "toto", "text not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#contains(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringStringString() {
        Assert.contains("tata part en vacances", "tutu", "text not found");
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#hasNoNullElements(Object[], String, Object...)}
     * .
     */
    @Test
    public void testIsNoNullElementOKsObjectArrayString() {
        try {
            String[] array = new String[] {"1", "3"};
            Assert.hasNoNullElements(array);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#hasNoNullElements(Object[], String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNoNullElementsKOObjectArrayString() {
        String[] array = new String[] {null, "2"};
        Assert.hasNoNullElements(array);
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#hasNoNullElements(java.lang.Object[])}
     * .
     */
    @Test
    public void testIsNoNullElementsOKObjectArray() {
        try {
            String[] array = new String[] {"1", "2"};
            Assert.hasNoNullElements(array, "array has null element");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link org.gl.utils.commons.Assert#hasNoNullElements(java.lang.Object[])}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNoNullElementsKOObjectArray() {
        String[] array = new String[] {"", null};
        Assert.hasNoNullElements(array, "array has null element");
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
     * Test method for {@link org.gl.utils.commons.Assert#that(Object, Matcher)}
     * .
     */
    @Test
    public void testThatOK() {
        try {
            final String red = "red";
            final String green = "green";
            final String blue = "blue";
            final String alpha = "alpha";

            final int max = 255;

            final int nbColors = 4;
            final List<Color> colors = new ArrayList<>(nbColors);

            colors.add(Color.BLACK);
            colors.add(Color.WHITE);
            colors.add(Color.BLUE);
            colors.add(Color.CYAN);

            Matcher<Color> matcherAlpha = Matchers.hasProperty(alpha, Matchers.is(max));

            Matcher<Color> matcherBlack = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(0)),
                    Matchers.hasProperty(red, Matchers.is(0)), Matchers.hasProperty(blue, Matchers.is(0)), matcherAlpha);

            Matcher<Color> matcherWhite = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(max)),
                    Matchers.hasProperty(red, Matchers.is(max)), Matchers.hasProperty(blue, Matchers.is(max)), matcherAlpha);

            Matcher<Color> matcherBlue = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(0)),
                    Matchers.hasProperty(red, Matchers.is(0)), Matchers.hasProperty(blue, Matchers.is(max)), matcherAlpha);

            Matcher<Color> matcherCyan = Matchers.allOf(Matchers.hasProperty(green, Matchers.is(max)),
                    Matchers.hasProperty(red, Matchers.is(0)), Matchers.hasProperty(blue, Matchers.is(max)), matcherAlpha);

            List<Matcher<? super Color>> matcherList = Arrays.<Matcher<? super Color>> asList(matcherBlack, matcherWhite, matcherBlue,
                    matcherCyan);

            Assert.that(colors, Matchers.hasSize(nbColors));
            Assert.that(colors, Matchers.contains(matcherList));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link org.gl.utils.commons.Assert#that(Object, Matcher)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testThatKO() {
        final List<Color> colors = new ArrayList<>();

        colors.add(Color.BLACK);
        colors.add(Color.WHITE);
        colors.add(Color.BLUE);
        colors.add(Color.CYAN);

        Assert.that(colors, Matchers.hasSize(colors.size() - 1));
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