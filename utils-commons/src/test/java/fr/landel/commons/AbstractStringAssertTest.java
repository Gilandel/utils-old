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
package fr.landel.commons;

import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import fr.landel.utils.commons.Assert;

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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(String, String, Object...)}
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOStringString() {
        Assert.isNotEmpty("", "empty string");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2StringString() {
        Assert.isNotEmpty((String) null, "empty string");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.lang.String)} .
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOString() {
        Assert.isNotEmpty("");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2String() {
        Assert.isNotEmpty((String) null);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(Object[], String, Object...)}
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(Object[], String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArrayString() {
        Assert.isNotEmpty(Collections.emptyList().toArray(), "empty array");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.lang.Object[])} .
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.lang.Object[])} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArray() {
        Assert.isNotEmpty(Collections.emptyList().toArray());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.util.Collection, String, Object...)}
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQString() {
        Assert.isNotEmpty(Collections.emptyList(), "empty collection");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.util.Collection)} .
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.util.Collection)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQ() {
        Assert.isNotEmpty(Collections.emptyList());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(Map, String, Object...)} .
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(Map, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQString() {
        Assert.isNotEmpty(new HashMap<String, String>(), "empty");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.util.Map)} .
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
     * {@link fr.landel.utils.commons.Assert#isNotEmpty(java.util.Map)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQ() {
        Assert.isNotEmpty(new HashMap<String, String>());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isEmpty(String, String, Object...)} .
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
     * {@link fr.landel.utils.commons.Assert#isEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOStringString() {
        Assert.isEmpty("r", "not empty");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isEmpty(java.lang.String)} .
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
     * {@link fr.landel.utils.commons.Assert#isEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOString() {
        Assert.isEmpty("e");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotBlank(String, String, Object...)}
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
     * {@link fr.landel.utils.commons.Assert#isNotBlank(String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOStringString() {
        Assert.isNotBlank("   \t    ", "blank");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isNotBlank(java.lang.String)} .
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
     * {@link fr.landel.utils.commons.Assert#isNotBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOString() {
        Assert.isNotBlank("    \t   ");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isBlank(String, String, Object...)} .
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
     * {@link fr.landel.utils.commons.Assert#isBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOStringString() {
        Assert.isBlank("   \t d   ", "not blank");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#isBlank(java.lang.String)} .
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
     * {@link fr.landel.utils.commons.Assert#isBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOString() {
        Assert.isBlank("      j ");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#doesNotContain(String, String, String, Object...)}
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
     * {@link fr.landel.utils.commons.Assert#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringStringString() {
        Assert.doesNotContain("titi part en vacances", "titi", "not found");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#doesNotContain(java.lang.String, java.lang.String)}
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
     * {@link fr.landel.utils.commons.Assert#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringString() {
        Assert.doesNotContain("tata part en vacances", "tata");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#contains(java.lang.String, java.lang.String)}
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
     * {@link fr.landel.utils.commons.Assert#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringString() {
        Assert.contains("tata part en vacances", "tutu");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.Assert#contains(String, String, String, Object...)}
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
     * {@link fr.landel.utils.commons.Assert#contains(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringStringString() {
        Assert.contains("tata part en vacances", "tutu", "text not found");
    }
}