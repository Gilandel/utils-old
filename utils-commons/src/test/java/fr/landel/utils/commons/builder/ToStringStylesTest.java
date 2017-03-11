/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.builder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check {@link ToStringStyles}
 *
 * @since Mar 11, 2017
 * @author Gilles
 *
 */
public class ToStringStylesTest {

    /**
     * Test method for {@link ToStringStyles#getSupplier()}.
     */
    @Test
    public void testGetSupplier() {
        assertTrue(ToStringStyleDefault.class.isAssignableFrom(ToStringStyles.DEFAULT.getSupplier().get().getClass()));
        assertTrue(ToStringStyleJSON.class.isAssignableFrom(ToStringStyles.JSON.getSupplier().get().getClass()));
        assertTrue(ToStringStyleJSONSpaced.class.isAssignableFrom(ToStringStyles.JSON_SPACED.getSupplier().get().getClass()));
        assertTrue(ToStringStyleJSONQuoted.class.isAssignableFrom(ToStringStyles.JSON_QUOTED.getSupplier().get().getClass()));
        assertTrue(ToStringStyleParenthesis.class.isAssignableFrom(ToStringStyles.PARENTHESIS.getSupplier().get().getClass()));
        assertTrue(ToStringStyleReadable.class.isAssignableFrom(ToStringStyles.READABLE.getSupplier().get().getClass()));

        assertEquals(ToStringStyles.DEFAULT, ToStringStyles.valueOf("DEFAULT"));
        assertEquals(0, ToStringStyles.DEFAULT.ordinal());
        assertEquals("DEFAULT", ToStringStyles.DEFAULT.name());
    }
}
