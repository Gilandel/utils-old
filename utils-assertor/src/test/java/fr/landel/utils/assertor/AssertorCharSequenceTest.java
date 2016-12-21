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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.regex.Pattern;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check {@link AssertorCharSequence}
 *
 * @since Dec 10, 2015
 * @author Gilles Landel
 *
 */
public class AssertorCharSequenceTest extends AbstractTest {

    /**
     * Test method for {@link AssertorCharSequence#AssertorCharSequence()} .
     */
    @Test
    public void testConstructor() {
        assertNotNull(new AssertorCharSequence());
    }

    /**
     * Test method for {@link AssertorCharSequence#hasLength(int)} .
     */
    @Test
    public void testHasLength() {
        assertTrue(Assertor.that("text").hasLength(4).isOK());
        assertFalse(Assertor.that("text").hasLength(3).isOK());
        assertFalse(Assertor.that("text").hasLength(-1).isOK());
        assertFalse(Assertor.that((String) null).hasLength(1).isOK());
    }

    /**
     * Test method for {@link AssertorCharSequence#hasLength(int)} .
     */
    @Test
    public void testHasNotLength() {
        assertFalse(Assertor.that("text").not().hasLength(4).isOK());
        assertTrue(Assertor.that("text").not().hasLength(3).isOK());
        assertFalse(Assertor.that("text").not().hasLength(-1).isOK());
        assertFalse(Assertor.that((String) null).not().hasLength(1).isOK());
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#isNotEmpty(String, String, Object...)} .
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
     * {@link AssertorCharSequence#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOStringString() {
        Assertor.that("").isNotEmpty().toThrow("empty string");
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKONot() {
        Assertor.that("z").not().isNotEmpty().toThrow("empty string");
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#isNotEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2StringString() {
        Assertor.that((String) null).isNotEmpty().toThrow("empty string");
    }

    /**
     * Test method for {@link AssertorCharSequence#isNotEmpty(java.lang.String)}
     * .
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
     * Test method for {@link AssertorCharSequence#isNotEmpty(java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOString() {
        Assertor.that("").isNotEmpty().toThrow();
    }

    /**
     * Test method for {@link AssertorCharSequence#isNotEmpty(java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKO2String() {
        Assertor.that((String) null).isNotEmpty().toThrow();
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#isEmpty(String, String, Object...)} .
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
     * {@link AssertorCharSequence#isEmpty(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOStringString() {
        Assertor.that("r").isEmpty().toThrow("not empty");
    }

    /**
     * Test method for {@link AssertorCharSequence#isEmpty(java.lang.String)} .
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
     * Test method for {@link AssertorCharSequence#isEmpty(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsEmptyKOString() {
        Assertor.that("e").isEmpty().toThrow();
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#isNotBlank(String, String, Object...)} .
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
     * {@link AssertorCharSequence#isNotBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOStringString() {
        Assertor.that("   \t    ").isNotBlank().toThrow("blank");
    }

    /**
     * Test method for {@link AssertorCharSequence#isNotBlank(java.lang.String)}
     * .
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
     * Test method for {@link AssertorCharSequence#isNotBlank(java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOString() {
        Assertor.that("    \t   ").isNotBlank().toThrow();
    }

    /**
     * Test method for {@link AssertorCharSequence#isNotBlank(java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotBlankKOnot() {
        Assertor.that("    \t   a").not().isNotBlank().toThrow();
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#isBlank(String, String, Object...)} .
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
     * {@link AssertorCharSequence#isBlank(String, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOStringString() {
        Assertor.that("   \t d   ").isBlank().toThrow("not blank");
    }

    /**
     * Test method for {@link AssertorCharSequence#isBlank(java.lang.String)} .
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
     * Test method for {@link AssertorCharSequence#isBlank(java.lang.String)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsBlankKOString() {
        Assertor.that("      j ").isBlank().toThrow();
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#contains(String, String, String, Object...)}
     * .
     */
    @Test
    public void testDoesNotContainOKStringStringString() {
        try {
            Assertor.that("titi part en vacances").not().contains("toto").toThrow("not found");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#contains(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringStringString() {
        Assertor.that("titi part en vacances").not().contains("titi").toThrow("not found");
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#not().contains(java.lang.String,
     * java.lang.String)} .
     */
    @Test
    public void testDoesNotContain() {
        assertTrue(Assertor.that("totos").not().contains("toto part en vacances").isOK());
        assertTrue(Assertor.that("toto").not().contains("totu").isOK());
        assertTrue(Assertor.that("toto").not().contains('x').isOK());
        assertFalse(Assertor.that("toto part en vacances").not().contains("toto").isOK());
        assertFalse(Assertor.that((String) null).not().contains("toto part en vacances").isOK());
        assertFalse(Assertor.that("toto").not().contains((CharSequence) null).isOK());
        assertFalse(Assertor.that("toto").not().contains((Character) null).isOK());
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testDoesNotContainKOStringString() {
        Assertor.that("tata part en vacances").not().contains("tata").toThrow();
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test
    public void testContains() {
        assertTrue(Assertor.that("toto part en vacances").contains("toto").isOK());
        assertTrue(Assertor.that("toto").contains('t').isOK());
        assertTrue(Assertor.that("toto").contains("toto").isOK());
        assertTrue(Assertor.that("toti et toto part en vacances").contains("toto").isOK());
        assertFalse(Assertor.that("toti part en vacances en moto").contains("toto").isOK());
        assertFalse(Assertor.that("toto").contains("toto part en vacances").isOK());
        assertFalse(Assertor.that((String) null).contains("toto part en vacances").isOK());
        assertFalse(Assertor.that("toto").contains((CharSequence) null).isOK());
        assertFalse(Assertor.that("toto").contains((Character) null).isOK());

        Expect.exception(() -> {
            Assertor.that("toto part en vacances").contains("toto").and().contains("voyage").toThrow();
        }, IllegalArgumentException.class, "the char sequence 'toto part en vacances' should contain 'voyage'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("toto part en vacances").contains("toto").and().contains("voyage")
                    .and(Assertor.that("text").isBlank().or().contains("text")).toThrow();
        }, IllegalArgumentException.class, "the char sequence 'toto part en vacances' should contain 'voyage'", JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("toto part en vacances").contains("toto").and().contains("voyage")
                    .or(Assertor.that("text").isBlank().or().not().contains("text")).toThrow();
        }, IllegalArgumentException.class,
                "the char sequence 'toto part en vacances' should contain 'voyage'"
                        + " OR (the char sequence 'text' should be null, empty or blank OR the char sequence 'text' should NOT contain 'text')",
                JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that("toto part en vacances").contains('t').and().contains('y').toThrow();
        }, IllegalArgumentException.class, "the char sequence 'toto part en vacances' should contain 'y'");

        Expect.exception(() -> {
            Assertor.that("toto part en vacances").contains('t').and().contains('y')
                    .and(Assertor.that("text").isBlank().or().contains("text")).toThrow();
        }, IllegalArgumentException.class, "the char sequence 'toto part en vacances' should contain 'y'");

        Expect.exception(() -> {
            Assertor.that("toto part en vacances").contains('t').and().contains('y')
                    .or(Assertor.that("text").isBlank().or().not().contains('t')).toThrow();
        }, IllegalArgumentException.class,
                "the char sequence 'toto part en vacances' should contain 'y'"
                        + " OR (the char sequence 'text' should be null, empty or blank OR the char sequence 'text' should NOT contain 't')",
                JUNIT_ERROR);

        Expect.exception(() -> {
            Assertor.that((CharSequence) null).contains('t').and().contains((Character) null).toThrow();
        }, IllegalArgumentException.class, "the char sequence cannot be null and the searched substring cannot be null or empty",
                JUNIT_ERROR);
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#contains(java.lang.String, java.lang.String)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringString() {
        Assertor.that("tata part en vacances").contains("tutu").toThrow();
    }

    /**
     * Test method for
     * {@link AssertorCharSequence#contains(String, String, String, Object...)}
     * .
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
     * {@link AssertorCharSequence#contains(String, String, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testContainsKOStringStringString() {
        Assertor.that("tata part en vacances").contains("tutu").toThrow("text not found");
    }

    /**
     * Test method for {@link AssertorCharSequence#startsWith} and
     * {@link AssertorCharSequence#startsWithIgnoreCase}.
     */
    @Test
    public void testStartsWith() {
        assertTrue(Assertor.that("TexT").startsWith("Tex").isOK());
        assertFalse(Assertor.that("TexT").startsWith("tex").isOK());
        assertFalse(Assertor.that("TexT").startsWith("ext").isOK());
        assertFalse(Assertor.that("TexT").startsWith("").isOK());
        assertFalse(Assertor.that("").startsWith("").isOK());
        assertFalse(Assertor.that("TexT").startsWith("Texte").isOK());
        assertFalse(Assertor.that((String) null).startsWith("Tex").isOK());
        assertFalse(Assertor.that("TexT").startsWith(null).isOK());

        assertTrue(Assertor.that("TexT").startsWithIgnoreCase("tex").isOK());
        assertTrue(Assertor.that("TexT").startsWithIgnoreCase("tex").isOK());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase("ext").isOK());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase("").isOK());
        assertFalse(Assertor.that("").startsWithIgnoreCase("").isOK());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase("texte").isOK());
        assertFalse(Assertor.that((String) null).startsWithIgnoreCase("tex").isOK());
        assertFalse(Assertor.that("TexT").startsWithIgnoreCase(null).isOK());
    }

    /**
     * Test method for {@link AssertorCharSequence#endsWith} and
     * {@link AssertorCharSequence#endsWithIgnoreCase}.
     */
    @Test
    public void testEndsWith() {
        assertTrue(Assertor.that("TexT").endsWith("exT").isOK());
        assertFalse(Assertor.that("TexT").endsWith("ext").isOK());
        assertFalse(Assertor.that("TexT").endsWith("tex").isOK());
        assertFalse(Assertor.that("TexT").endsWith("").isOK());
        assertFalse(Assertor.that("").endsWith("").isOK());
        assertFalse(Assertor.that("TexT").endsWith("eTexT").isOK());
        assertFalse(Assertor.that((String) null).endsWith("exT").isOK());
        assertFalse(Assertor.that("TexT").endsWith(null).isOK());

        assertTrue(Assertor.that("TexT").endsWithIgnoreCase("exT").isOK());
        assertTrue(Assertor.that("TexT").endsWithIgnoreCase("ext").isOK());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase("tex").isOK());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase("").isOK());
        assertFalse(Assertor.that("").endsWithIgnoreCase("").isOK());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase("eTexT").isOK());
        assertFalse(Assertor.that((String) null).endsWithIgnoreCase("exT").isOK());
        assertFalse(Assertor.that("TexT").endsWithIgnoreCase(null).isOK());
    }

    /**
     * Test method for {@link AssertorCharSequence#matches}.
     */
    @Test
    public void testMatches() {
        final String regex = "[xeT]{4}";
        final Pattern pattern = Pattern.compile(regex);

        assertTrue(Assertor.that("TexT").matches(pattern).isOK());
        assertFalse(Assertor.that("Text").matches(pattern).isOK());
        assertFalse(Assertor.that((String) null).matches(pattern).isOK());
        assertFalse(Assertor.that("Text").matches((Pattern) null).isOK());

        assertTrue(Assertor.that("TexT").matches(regex).isOK());
        assertFalse(Assertor.that("Text").matches(regex).isOK());
        assertFalse(Assertor.that((String) null).matches(regex).isOK());
        assertFalse(Assertor.that("Text").matches((String) null).isOK());
    }

    /**
     * Test method for {@link AssertorCharSequence#find}.
     */
    @Test
    public void testFind() {
        final String regex = "[xeT]{3}";
        final Pattern pattern = Pattern.compile(regex);

        assertTrue(Assertor.that("TexT").find(pattern).isOK());
        assertTrue(Assertor.that("Text").find(pattern).isOK());
        assertFalse(Assertor.that("Tetxt").find(pattern).isOK());
        assertFalse(Assertor.that((String) null).find(pattern).isOK());
        assertFalse(Assertor.that("Text").find((Pattern) null).isOK());

        assertTrue(Assertor.that("TexT").find(regex).isOK());
        assertTrue(Assertor.that("Text").find(regex).isOK());
        assertFalse(Assertor.that("Tetxt").find(regex).isOK());
        assertFalse(Assertor.that((String) null).find(regex).isOK());
        assertFalse(Assertor.that("Text").find((String) null).isOK());
        assertFalse(Assertor.that((String) null).find((String) null).isOK());
    }
}
