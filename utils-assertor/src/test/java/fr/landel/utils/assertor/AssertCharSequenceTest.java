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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.regex.Pattern;

import org.junit.Test;

/**
 * Check assert
 *
 * @since 10 dec. 2015
 * @author Gilles Landel
 *
 */
public class AssertCharSequenceTest {

    /**
     * Test method for {@link AssertCharSequence#hasLength(int)} .
     */
    @Test
    public void testHasLength() {
        assertTrue(Assertor.that("text").hasLength(4).getResult());
        assertFalse(Assertor.that("text").hasLength(3).getResult());
        assertFalse(Assertor.that("text").hasLength(-1).getResult());
        assertFalse(Assertor.that((String) null).hasLength(1).getResult());
    }

    /**
     * Test method for {@link AssertCharSequence#hasNotLength(int)} .
     */
    @Test
    public void testHasNotLength() {
        assertFalse(Assertor.that("text").hasNotLength(4).getResult());
        assertTrue(Assertor.that("text").hasNotLength(3).getResult());
        assertFalse(Assertor.that("text").hasNotLength(-1).getResult());
        assertFalse(Assertor.that((String) null).hasNotLength(1).getResult());
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isNotEmpty(String, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKStringString() {
        try {
            Assertor.that("a").isNotEmpty().toThrow("empty string");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOStringString() {
        Assertor.that("").isNotEmpty().toThrow("empty string");
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2StringString() {
        Assertor.that((String) null).isNotEmpty().toThrow("empty string");
    }

    /**
     * Test method for {@link AssertCharSequence#isNotEmpty(java.lang.String)} .
     */
    @Test
    public void testIsNotEmptyOKString() {
        try {
            Assertor.that("z").isNotEmpty().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertCharSequence#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOString() {
        Assertor.that("").isNotEmpty().toThrow();
    }

    /**
     * Test method for {@link AssertCharSequence#isNotEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2String() {
        Assertor.that((String) null).isNotEmpty().toThrow();
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isEmpty(String, String, Object...)} .
     */
    @Test
    public void testIsEmptyOKStringString() {
        try {
            Assertor.that((String) null).isEmpty().toThrow("not empty or null");
            Assertor.that("").isEmpty().toThrow("not empty");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOStringString() {
        Assertor.that("r").isEmpty().toThrow("not empty");
    }

    /**
     * Test method for {@link AssertCharSequence#isEmpty(java.lang.String)} .
     */
    @Test
    public void testIsEmptyOKString() {
        try {
            Assertor.that((String) null).isEmpty().toThrow();
            Assertor.that("").isEmpty().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertCharSequence#isEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOString() {
        Assertor.that("e").isEmpty().toThrow();
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isNotBlank(String, String, Object...)} .
     */
    @Test
    public void testIsNotBlankOKStringString() {
        try {
            Assertor.that("   \t sds  ").isNotBlank().toThrow("blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isNotBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOStringString() {
        Assertor.that("   \t    ").isNotBlank().toThrow("blank");
    }

    /**
     * Test method for {@link AssertCharSequence#isNotBlank(java.lang.String)} .
     */
    @Test
    public void testIsNotBlankOKString() {
        try {
            Assertor.that("    \t  e ").isNotBlank().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertCharSequence#isNotBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOString() {
        Assertor.that("    \t   ").isNotBlank().toThrow();
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isBlank(String, String, Object...)} .
     */
    @Test
    public void testIsBlankOKStringString() {
        try {
            Assertor.that("   \t   ").isBlank().toThrow("not blank");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertCharSequence#isBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOStringString() {
        Assertor.that("   \t d   ").isBlank().toThrow("not blank");
    }

    /**
     * Test method for {@link AssertCharSequence#isBlank(java.lang.String)} .
     */
    @Test
    public void testIsBlankOKString() {
        try {
            Assertor.that("   \t   ").isBlank().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertCharSequence#isBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOString() {
        Assertor.that("      j ").isBlank().toThrow();
    }

    /**
     * Test method for
     * {@link AssertCharSequence#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test
    public void testDoesNotContainOKStringStringString() {
        try {
            Assertor.that("titi part en vacances").doesNotContain("toto").toThrow("not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertCharSequence#doesNotContain(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringStringString() {
        Assertor.that("titi part en vacances").doesNotContain("titi").toThrow("not found");
    }

    /**
     * Test method for
     * {@link AssertCharSequence#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testDoesNotContain() {
        assertTrue(Assertor.that("totos").doesNotContain("toto part en vacances").getResult());
        assertTrue(Assertor.that("toto").doesNotContain("totu").getResult());
        assertFalse(Assertor.that("toto part en vacances").doesNotContain("toto").getResult());
        assertFalse(Assertor.that((String) null).doesNotContain("toto part en vacances").getResult());
        assertFalse(Assertor.that("toto").doesNotContain(null).getResult());
    }

    /**
     * Test method for
     * {@link AssertCharSequence#doesNotContain(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringString() {
        Assertor.that("tata part en vacances").doesNotContain("tata").toThrow();
    }

    /**
     * Test method for
     * {@link AssertCharSequence#contains(java.lang.String, java.lang.String)} .
     */
    @Test
    public void testContains() {
        assertTrue(Assertor.that("toto part en vacances").contains("toto").getResult());
        assertTrue(Assertor.that("toto").contains("toto").getResult());
        assertFalse(Assertor.that("toto").contains("toto part en vacances").getResult());
        assertFalse(Assertor.that((String) null).contains("toto part en vacances").getResult());
        assertFalse(Assertor.that("toto").contains(null).getResult());
    }

    /**
     * Test method for
     * {@link AssertCharSequence#contains(java.lang.String, java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringString() {
        Assertor.that("tata part en vacances").contains("tutu").toThrow();
    }

    /**
     * Test method for
     * {@link AssertCharSequence#contains(String, String, String, Object...)} .
     */
    @Test
    public void testContainsOKStringStringString() {
        try {
            Assertor.that("toto part en vacances").contains("toto").toThrow("text not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertCharSequence#contains(String, String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringStringString() {
        Assertor.that("tata part en vacances").contains("tutu").toThrow("text not found");
    }

    /**
     * Test method for {@link AssertCharSequence#startsWith} and
     * {@link AssertCharSequence#startsWithIgnoreCase}.
     */
    @Test
    public void testStartsWith() {
        assertTrue(Assertor.that("TexT").startsWith("Tex").getResult());
        assertFalse(Assertor.that("TexT").startsWith("tex").getResult());
        assertFalse(Assertor.that("TexT").startsWith("ext").getResult());
        assertFalse(Assertor.that("TexT").startsWith("").getResult());
        assertFalse(Assertor.that("").startsWith("").getResult());
        assertFalse(Assertor.that("TexT").startsWith("Texte").getResult());
        assertFalse(Assertor.that((String) null).startsWith("Tex").getResult());
        assertFalse(Assertor.that("TexT").startsWith(null).getResult());

        assertTrue(Assertor.that("TexT").startsWithIgnoreCase("tex").getResult());
        assertTrue(Assertor.that("TexT").startsWithIgnoreCase("tex").getResult());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase("ext").getResult());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase("").getResult());
        assertFalse(Assertor.that("").startsWithIgnoreCase("").getResult());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase("texte").getResult());
        assertFalse(Assertor.that((String) null).startsWithIgnoreCase("tex").getResult());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase(null).getResult());
    }

    /**
     * Test method for {@link AssertCharSequence#endsWith} and
     * {@link AssertCharSequence#endsWithIgnoreCase}.
     */
    @Test
    public void testEndsWith() {
        assertTrue(Assertor.that("TexT").endsWith("exT").getResult());
        assertFalse(Assertor.that("TexT").endsWith("ext").getResult());
        assertFalse(Assertor.that("TexT").endsWith("tex").getResult());
        assertFalse(Assertor.that("TexT").endsWith("").getResult());
        assertFalse(Assertor.that("").endsWith("").getResult());
        assertFalse(Assertor.that("TexT").endsWith("eTexT").getResult());
        assertFalse(Assertor.that((String) null).endsWith("exT").getResult());
        assertFalse(Assertor.that("TexT").endsWith(null).getResult());

        assertTrue(Assertor.that("TexT").endsWithIgnoreCase("exT").getResult());
        assertTrue(Assertor.that("TexT").endsWithIgnoreCase("ext").getResult());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase("tex").getResult());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase("").getResult());
        assertFalse(Assertor.that("").endsWithIgnoreCase("").getResult());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase("eTexT").getResult());
        assertFalse(Assertor.that((String) null).endsWithIgnoreCase("exT").getResult());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase(null).getResult());
    }

    /**
     * Test method for {@link AssertCharSequence#matches}.
     */
    @Test
    public void testMatches() {
        final String regex = "[xeT]{4}";
        final Pattern pattern = Pattern.compile(regex);

        assertTrue(Assertor.that("TexT").matches(pattern).getResult());
        assertFalse(Assertor.that("Text").matches(pattern).getResult());
        assertFalse(Assertor.that((String) null).matches(pattern).getResult());
        assertFalse(Assertor.that("Text").matches((Pattern) null).getResult());

        assertTrue(Assertor.that("TexT").matches(regex).getResult());
        assertFalse(Assertor.that("Text").matches(regex).getResult());
        assertFalse(Assertor.that((String) null).matches(regex).getResult());
        assertFalse(Assertor.that("Text").matches((String) null).getResult());
    }

    /**
     * Test method for {@link AssertCharSequence#find}.
     */
    @Test
    public void testFind() {
        final String regex = "[xeT]{3}";
        final Pattern pattern = Pattern.compile(regex);

        assertTrue(Assertor.that("TexT").find(pattern).getResult());
        assertTrue(Assertor.that("Text").find(pattern).getResult());
        assertFalse(Assertor.that("Tetxt").find(pattern).getResult());
        assertFalse(Assertor.that((String) null).find(pattern).getResult());
        assertFalse(Assertor.that("Text").find((Pattern) null).getResult());

        assertTrue(Assertor.that("TexT").find(regex).getResult());
        assertTrue(Assertor.that("Text").find(regex).getResult());
        assertFalse(Assertor.that("Tetxt").find(regex).getResult());
        assertFalse(Assertor.that((String) null).find(regex).getResult());
        assertFalse(Assertor.that("Text").find((String) null).getResult());
        assertFalse(Assertor.that((String) null).find((String) null).getResult());
    }
}