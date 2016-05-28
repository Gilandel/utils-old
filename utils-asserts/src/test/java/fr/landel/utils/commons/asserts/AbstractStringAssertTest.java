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

import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import fr.landel.commons.asserts.AssertUtils;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AbstractStringAssertTest {

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKStringString() {
        try {
            AssertUtils.isNotEmpty("a", "empty string");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOStringString() {
        AssertUtils.isNotEmpty("", "empty string");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2StringString() {
        AssertUtils.isNotEmpty((String) null, "empty string");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.lang.String)} .
     */
    @Test
    public void testIsNotEmptyOKString() {
        try {
            AssertUtils.isNotEmpty("z");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOString() {
        AssertUtils.isNotEmpty("");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2String() {
        AssertUtils.isNotEmpty((String) null);
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(Object[], String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKObjectArrayString() {
        try {
            AssertUtils.isNotEmpty(Arrays.asList("").toArray(), "empty array");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(Object[], String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArrayString() {
        AssertUtils.isNotEmpty(Collections.emptyList().toArray(), "empty array");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.lang.Object[])} .
     */
    @Test
    public void testIsNotEmptyOKObjectArray() {
        try {
            AssertUtils.isNotEmpty(Arrays.asList("").toArray());
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.lang.Object[])} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArray() {
        AssertUtils.isNotEmpty(Collections.emptyList().toArray());
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQString() {
        try {
            AssertUtils.isNotEmpty(Arrays.asList(""), "empty collection");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQString() {
        AssertUtils.isNotEmpty(Collections.emptyList(), "empty collection");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.util.Collection)} .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQ() {
        try {
            AssertUtils.isNotEmpty(Arrays.asList(""));
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.util.Collection)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQ() {
        AssertUtils.isNotEmpty(Collections.emptyList());
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(Map, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQString() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("f", "f");
            AssertUtils.isNotEmpty(map, "empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(Map, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQString() {
        AssertUtils.isNotEmpty(new HashMap<String, String>(), "empty");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.util.Map)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQ() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("fg", "fg");
            AssertUtils.isNotEmpty(map);
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotEmpty(java.util.Map)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQ() {
        AssertUtils.isNotEmpty(new HashMap<String, String>());
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isEmpty(String, String, Object...)} .
     */
    @Test
    public void testIsEmptyOKStringString() {
        try {
            AssertUtils.isEmpty(null, "not empty or null");
            AssertUtils.isEmpty("", "not empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOStringString() {
        AssertUtils.isEmpty("r", "not empty");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isEmpty(java.lang.String)} .
     */
    @Test
    public void testIsEmptyOKString() {
        try {
            AssertUtils.isEmpty(null);
            AssertUtils.isEmpty("");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOString() {
        AssertUtils.isEmpty("e");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotBlank(String, String, Object...)}
     * .
     */
    @Test
    public void testIsNotBlankOKStringString() {
        try {
            AssertUtils.isNotBlank("   \t sds  ", "blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotBlank(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOStringString() {
        AssertUtils.isNotBlank("   \t    ", "blank");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotBlank(java.lang.String)} .
     */
    @Test
    public void testIsNotBlankOKString() {
        try {
            AssertUtils.isNotBlank("    \t  e ");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isNotBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOString() {
        AssertUtils.isNotBlank("    \t   ");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isBlank(String, String, Object...)} .
     */
    @Test
    public void testIsBlankOKStringString() {
        try {
            AssertUtils.isBlank("   \t   ", "not blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOStringString() {
        AssertUtils.isBlank("   \t d   ", "not blank");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isBlank(java.lang.String)} .
     */
    @Test
    public void testIsBlankOKString() {
        try {
            AssertUtils.isBlank("   \t   ");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#isBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOString() {
        AssertUtils.isBlank("      j ");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test
    public void testDoesNotContainOKStringStringString() {
        try {
            AssertUtils.doesNotContain("titi part en vacances", "toto", "not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringStringString() {
        AssertUtils.doesNotContain("titi part en vacances", "titi", "not found");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testDoesNotContainOKStringString() {
        try {
            AssertUtils.doesNotContain("toto part en vacances", "tutu");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringString() {
        AssertUtils.doesNotContain("tata part en vacances", "tata");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testContainsOKStringString() {
        try {
            AssertUtils.contains("toto part en vacances", "toto");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringString() {
        AssertUtils.contains("tata part en vacances", "tutu");
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#contains(String, String, String, Object...)}
     * .
     */
    @Test
    public void testContainsOKStringStringString() {
        try {
            AssertUtils.contains("toto part en vacances", "toto", "text not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link fr.landel.commons.asserts.AssertUtils#contains(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringStringString() {
        AssertUtils.contains("tata part en vacances", "tutu", "text not found");
    }
}
