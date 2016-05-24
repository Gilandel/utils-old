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
package fr.landel.utils.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import fr.landel.utils.commons.StringUtils;

/**
 * Check utility class (strings).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class StringUtilsTest {

    /**
     * Test method for
     * {@link fr.landel.utils.commons.StringUtils#getNullIfEmpty(java.lang.CharSequence)}
     * .
     */
    @Test
    public void testGetNullIfEmpty() {
        String expected = "value";
        assertEquals(expected, StringUtils.getNullIfEmpty(expected));

        assertNull(StringUtils.getNullIfEmpty(""));
        assertNull(StringUtils.getNullIfEmpty(null));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.StringUtils#getDefaultIfEmpty(java.lang.CharSequence, java.lang.CharSequence)}
     * .
     */
    @Test
    public void testGetDefaultIfEmpty() {
        String expected = "value";
        String defaultValue = "def";
        assertEquals(expected, StringUtils.getDefaultIfEmpty(expected, defaultValue));

        assertEquals(defaultValue, StringUtils.getDefaultIfEmpty("", defaultValue));
        assertEquals(defaultValue, StringUtils.getDefaultIfEmpty(null, defaultValue));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.StringUtils#getDefaultIfNull(java.lang.CharSequence, java.lang.CharSequence)}
     * .
     */
    @Test
    public void testGetDefaultIfNull() {
        String expected = "value";
        String defaultValue = "def";
        assertEquals(expected, StringUtils.getDefaultIfNull(expected, defaultValue));

        assertEquals("", StringUtils.getDefaultIfNull("", defaultValue));
        assertEquals(defaultValue, StringUtils.getDefaultIfNull(null, defaultValue));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.StringUtils#getToStringOrDefaultIfNull(java.lang.Object, java.lang.String)}
     * .
     */
    @Test
    public void testGetToStringOrDefaultIfNull() {
        Long expected = 1L;
        String defaultValue = "def";
        assertEquals(String.valueOf(expected), StringUtils.getToStringOrDefaultIfNull(expected, defaultValue));

        assertEquals("", StringUtils.getToStringOrDefaultIfNull("", defaultValue));
        assertEquals(defaultValue, StringUtils.getToStringOrDefaultIfNull(null, defaultValue));
    }

    /**
     * Test method for
     * {@link StringUtils#substring(java.lang.Object, char, int)} .
     * {@link StringUtils#substring(java.lang.Object, char, int, int)} .
     */
    @Test
    public void testSubtring() {
        String str = "test:toto:titi:";

        assertEquals(str, StringUtils.substring(str, "!", 0));

        str = "test:toto:titi:";

        assertEquals("test", StringUtils.substring(str, ":", 0));
        assertEquals("toto", StringUtils.substring(str, ":", 1));
        assertEquals("titi", StringUtils.substring(str, ":", 2));
        // exists
        assertEquals("", StringUtils.substring(str, ":", 3));
        // out of bounds
        assertEquals("", StringUtils.substring(str, ":", 4));

        // exists
        assertEquals("", StringUtils.substring(str, ":", -1));
        assertEquals("titi", StringUtils.substring(str, ":", -2));
        assertEquals("toto", StringUtils.substring(str, ":", -3));
        assertEquals("test", StringUtils.substring(str, ":", -4));
        // out of bounds
        assertEquals("", StringUtils.substring(str, ":", -5));

        assertEquals("test:toto", StringUtils.substring(str, ":", 0, 2));
        assertEquals("test:toto:titi", StringUtils.substring(str, ":", 0, 3));
        assertEquals("test:toto:titi:", StringUtils.substring(str, ":", 0, 4));
        // last is out of bounds
        assertEquals("test:toto:titi:", StringUtils.substring(str, ":", 0, 5));

        assertEquals("titi:", StringUtils.substring(str, ":", -1, -3));
        assertEquals("toto:titi:", StringUtils.substring(str, ":", -1, -4));
        assertEquals("test:toto:titi:", StringUtils.substring(str, ":", -1, -5));
        assertEquals("test:toto:titi:", StringUtils.substring(str, ":", -1, -6));

        assertEquals("toto", StringUtils.substring(str, ":", 1, 2));
        assertEquals("toto:titi", StringUtils.substring(str, ":", 1, 3));
        assertEquals("toto:titi:", StringUtils.substring(str, ":", 1, 4));
        assertEquals("toto:titi:", StringUtils.substring(str, ":", 1, 5));

        assertEquals("titi", StringUtils.substring(str, ":", -2, -3));
        assertEquals("toto:titi", StringUtils.substring(str, ":", -2, -4));
        assertEquals("test:toto:titi", StringUtils.substring(str, ":", -2, -5));
        assertEquals("test:toto:titi", StringUtils.substring(str, ":", -2, -6));

        str = "test::toto:titi::";

        assertEquals("test", StringUtils.substring(str, "::", 0));
        assertEquals("toto:titi", StringUtils.substring(str, "::", 1));
        assertEquals("", StringUtils.substring(str, "::", 2));
        assertEquals("", StringUtils.substring(str, "::", 3));
        assertEquals("", StringUtils.substring(str, "::", 4));

        assertEquals("", StringUtils.substring(str, "::", -1));
        assertEquals("toto:titi", StringUtils.substring(str, "::", -2));
        assertEquals("test", StringUtils.substring(str, "::", -3));
        assertEquals("", StringUtils.substring(str, "::", -4));
        assertEquals("", StringUtils.substring(str, "::", -5));

        assertEquals("test::toto:titi", StringUtils.substring(str, "::", 0, 2));
        assertEquals("test::toto:titi::", StringUtils.substring(str, "::", 0, 3));
        assertEquals("test::toto:titi::", StringUtils.substring(str, "::", 0, 4));
        assertEquals("test::toto:titi::", StringUtils.substring(str, "::", 0, 5));

        assertEquals("toto:titi::", StringUtils.substring(str, "::", -1, -3));
        assertEquals("test::toto:titi::", StringUtils.substring(str, "::", -1, -4));
        assertEquals("test::toto:titi::", StringUtils.substring(str, "::", -1, -5));
        assertEquals("test::toto:titi::", StringUtils.substring(str, "::", -1, -6));

        assertEquals("toto:titi", StringUtils.substring(str, "::", 1, 2));
        assertEquals("toto:titi::", StringUtils.substring(str, "::", 1, 3));
        assertEquals("toto:titi::", StringUtils.substring(str, "::", 1, 4));
        assertEquals("toto:titi::", StringUtils.substring(str, "::", 1, 5));

        assertEquals("toto:titi", StringUtils.substring(str, "::", -2, -3));
        assertEquals("test::toto:titi", StringUtils.substring(str, "::", -2, -4));
        assertEquals("test::toto:titi", StringUtils.substring(str, "::", -2, -5));
        assertEquals("test::toto:titi", StringUtils.substring(str, "::", -2, -6));

        assertEquals("toto:titi", StringUtils.substring(str, "::", 1, -1));

        assertEquals("test", StringUtils.substring(str, "::", 0, -2));

        // to not checked
        assertEquals("", StringUtils.substring(str, "::", 100, 10));
    }

    /**
     * Test method for
     * {@link StringUtils#substring(java.lang.Object, char, int, int)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testSubtringException() {
        String str = "test:toto:titi:";

        // from=0, to=0
        StringUtils.substring(str, ":", 0, 0);
    }

    /**
     * Test method for
     * {@link StringUtils#substring(java.lang.Object, char, int, int)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testSubtringException2() {
        String str = "test::toto:titi::";

        // from=0, to=0
        StringUtils.substring(str, "::", 1, -5);
    }

    /**
     * Test method for
     * {@link StringUtils#substring(java.lang.Object, char, int, int)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testSubtringException3() {
        String str = "test::toto:titi::";

        // from=0, to=0
        StringUtils.substring(str, "::", 1, -2);
    }

    /**
     * Test method for
     * {@link StringUtils#substring(java.lang.Object, char, int, int)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testSubtringException4() {
        String str = "test::toto:titi::";

        // from > to (negative, so starts from right)
        StringUtils.substring(str, "::", -2, -1);
    }

    /**
     * Test method for
     * {@link StringUtils#substring(java.lang.Object, char, int, int)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testSubtringException5() {
        String str = "test::toto:titi::";

        // from > to (positive, so starts from left)
        StringUtils.substring(str, "::", 2, 1);
    }

    /**
     * Test method for
     * {@link StringUtils#replace(java.lang.String, java.lang.String, int, int)}
     * .
     */
    @Test
    public void testReplace() {
        String string = "I go to the beach this afternoon.";
        assertEquals("I go to the theater this afternoon.", StringUtils.replace(string, "theater", 12, 17));
        assertEquals("I will go to the beach this afternoon.", StringUtils.replace(string, "I will", 0, 1));
        assertEquals("I go to the beach this morning.", StringUtils.replace(string, "morning", 23, 32));
    }

    /**
     * Test method for
     * {@link StringUtils#replace(java.lang.String, java.lang.String, int, int)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testReplaceException() {
        final String string = "I go to the beach this afternoon.";
        StringUtils.replace(string, "theater", -1, 1);
    }

    /**
     * Test method for
     * {@link StringUtils#replace(java.lang.String, java.lang.String, int, int)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testReplaceException2() {
        final String string = "I go to the beach this afternoon.";
        StringUtils.replace(string, "theater", 0, 34);
    }

    /**
     * Test method for
     * {@link StringUtils#replace(java.lang.String, java.lang.String, int, int)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testReplaceException3() {
        final String string = "I go to the beach this afternoon.";
        StringUtils.replace(string, null, 0, 1);
    }

    /**
     * Test method for
     * {@link StringUtils#replace(java.lang.String, java.lang.String, int, int)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testReplaceException4() {
        StringUtils.replace(null, "theater", 0, 1);
    }

    /**
     * Test method for
     * {@link StringUtils#replace(java.lang.String, java.lang.String, int, int)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testReplaceException5() {
        final String string = "I go to the beach this afternoon.";
        StringUtils.replace(string, "theater", 2, 1);
    }
}
