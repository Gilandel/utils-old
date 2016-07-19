/*-
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

import org.junit.Test;

/**
 * Check {@link EnumChar}
 *
 * @since 31 mai 2016
 * @author Gilles
 *
 */
public class EnumCharTest {

    /**
     * Test method for {@link EnumChar#EnumChar(EnumChar)}.
     */
    @Test
    public void testEnumCharEnumChar() {
        assertEquals(EnumChar.DOT.getCharacter(), EnumChar.PERIOD.getCharacter());
        assertEquals(EnumChar.DOT.getCode(), EnumChar.PERIOD.getCode());
        assertEquals(EnumChar.DOT.getHTML(), EnumChar.PERIOD.getHTML());
        assertEquals(EnumChar.DOT.getUnicode(), EnumChar.PERIOD.getUnicode());
    }

    /**
     * Test method for {@link EnumChar#getCharacter()}.
     */
    @Test
    public void testGetCharacter() {
        assertEquals('@', EnumChar.AT.getCharacter());
    }

    /**
     * Test method for {@link EnumChar#getCode()}.
     */
    @Test
    public void testGetCode() {
        assertEquals(64, EnumChar.AT.getCode());
    }

    /**
     * Test method for {@link EnumChar#getUnicode()}.
     */
    @Test
    public void testGetUnicode() {
        assertEquals("@", EnumChar.AT.getUnicode());
        assertEquals("\u0040", EnumChar.AT.getUnicode());
    }

    /**
     * Test method for {@link EnumChar#getHTML()}.
     */
    @Test
    public void testGetHTML() {
        assertEquals("&commat;", EnumChar.AT.getHTML());
    }

    /**
     * Test method for {@link EnumChar#toString()}.
     */
    @Test
    public void testToString() {
        assertEquals("@", EnumChar.AT.toString());
    }
}
