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

import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AbstractStringAssertTest {

    /**
     * Test method for {@link Expect#isNotEmpty(String, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKStringString() {
        try {
            AssertUtils.check("a").isNotEmpty("empty string");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOStringString() {
        AssertUtils.check("").isNotEmpty("empty string");
    }

    /**
     * Test method for {@link Expect#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2StringString() {
        AssertUtils.check((String) null).isNotEmpty("empty string");
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.lang.String)} .
     */
    @Test
    public void testIsNotEmptyOKString() {
        try {
            AssertUtils.check("z").isNotEmpty();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOString() {
        AssertUtils.check("").isNotEmpty();
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2String() {
        AssertUtils.check((String) null).isNotEmpty();
    }

    /**
     * Test method for {@link Expect#isNotEmpty(Object[], String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKObjectArrayString() {
        try {
            AssertUtils.check(Arrays.asList("").toArray()).isNotEmpty("empty array");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(Object[], String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArrayString() {
        AssertUtils.check(Collections.emptyList().toArray()).isNotEmpty("empty array");
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.lang.Object[])} .
     */
    @Test
    public void testIsNotEmptyOKObjectArray() {
        try {
            AssertUtils.check(Arrays.asList("").toArray()).isNotEmpty();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.lang.Object[])} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOObjectArray() {
        AssertUtils.check(Collections.emptyList().toArray()).isNotEmpty();
    }

    /**
     * Test method for
     * {@link Expect#isNotEmpty(java.util.Collection, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQString() {
        try {
            AssertUtils.check(Arrays.asList("")).isNotEmpty("empty collection");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#isNotEmpty(java.util.Collection, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQString() {
        AssertUtils.check(Collections.emptyList()).isNotEmpty("empty collection");
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.util.Collection)} .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQ() {
        try {
            AssertUtils.check(Arrays.asList("")).isNotEmpty();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.util.Collection)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQ() {
        AssertUtils.check(Collections.emptyList()).isNotEmpty();
    }

    /**
     * Test method for {@link Expect#isNotEmpty(Map, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQString() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("f", "f");
            AssertUtils.check(map).isNotEmpty("empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(Map, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQString() {
        AssertUtils.check(new HashMap<String, String>()).isNotEmpty("empty");
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.util.Map)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQ() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("fg", "fg");
            AssertUtils.check(map).isNotEmpty();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotEmpty(java.util.Map)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQ() {
        AssertUtils.check(new HashMap<String, String>()).isNotEmpty();
    }

    /**
     * Test method for {@link Expect#isEmpty(String, String, Object...)} .
     */
    @Test
    public void testIsEmptyOKStringString() {
        try {
            AssertUtils.check((String) null).isEmpty("not empty or null");
            AssertUtils.check("").isEmpty("not empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOStringString() {
        AssertUtils.check("r").isEmpty("not empty");
    }

    /**
     * Test method for {@link Expect#isEmpty(java.lang.String)} .
     */
    @Test
    public void testIsEmptyOKString() {
        try {
            AssertUtils.check((String) null).isEmpty();
            AssertUtils.check("").isEmpty();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOString() {
        AssertUtils.check("e").isEmpty();
    }

    /**
     * Test method for {@link Expect#isNotBlank(String, String, Object...)} .
     */
    @Test
    public void testIsNotBlankOKStringString() {
        try {
            AssertUtils.check("   \t sds  ").isNotBlank("blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOStringString() {
        AssertUtils.check("   \t    ").isNotBlank("blank");
    }

    /**
     * Test method for {@link Expect#isNotBlank(java.lang.String)} .
     */
    @Test
    public void testIsNotBlankOKString() {
        try {
            AssertUtils.check("    \t  e ").isNotBlank();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isNotBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOString() {
        AssertUtils.check("    \t   ").isNotBlank();
    }

    /**
     * Test method for {@link Expect#isBlank(String, String, Object...)} .
     */
    @Test
    public void testIsBlankOKStringString() {
        try {
            AssertUtils.check("   \t   ").isBlank("not blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOStringString() {
        AssertUtils.check("   \t d   ").isBlank("not blank");
    }

    /**
     * Test method for {@link Expect#isBlank(java.lang.String)} .
     */
    @Test
    public void testIsBlankOKString() {
        try {
            AssertUtils.check("   \t   ").isBlank();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link Expect#isBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOString() {
        AssertUtils.check("      j ").isBlank();
    }

    /**
     * Test method for
     * {@link Expect#doesNotContain(String, String, String, Object...)} .
     */
    @Test
    public void testDoesNotContainOKStringStringString() {
        try {
            AssertUtils.check("titi part en vacances").doesNotContain("toto", "not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#doesNotContain(String, String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringStringString() {
        AssertUtils.check("titi part en vacances").doesNotContain("titi", "not found");
    }

    /**
     * Test method for
     * {@link Expect#doesNotContain(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testDoesNotContainOKStringString() {
        try {
            AssertUtils.check("toto part en vacances").doesNotContain("tutu");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#doesNotContain(java.lang.String, java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringString() {
        AssertUtils.check("tata part en vacances").doesNotContain("tata");
    }

    /**
     * Test method for
     * {@link Expect#contains(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testContainsOKStringString() {
        try {
            AssertUtils.check("toto part en vacances").contains("toto");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#contains(java.lang.String, java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringString() {
        AssertUtils.check("tata part en vacances").contains("tutu");
    }

    /**
     * Test method for
     * {@link Expect#contains(String, String, String, Object...)} .
     */
    @Test
    public void testContainsOKStringStringString() {
        try {
            AssertUtils.check("toto part en vacances").contains("toto", "text not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link Expect#contains(String, String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringStringString() {
        AssertUtils.check("tata part en vacances").contains("tutu", "text not found");
    }
}
