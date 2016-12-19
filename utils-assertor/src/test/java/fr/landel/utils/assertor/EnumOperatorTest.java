/*-
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

/**
 * Check {@link EnumOperator}
 *
 * @since 27 nov. 2016
 * @author Gilles
 *
 */
public class EnumOperatorTest {

    /**
     * Test method for {@link EnumOperator}.
     */
    @Test
    public void test() {
        assertNotNull(EnumOperator.values());
        assertEquals(3, EnumOperator.values().length);

        assertEquals(0, EnumOperator.AND.ordinal());
        assertEquals("operator.and", EnumOperator.AND.getKey());
        assertEquals("AND", EnumOperator.AND.name());
        assertEquals(1, EnumOperator.OR.ordinal());
        assertEquals("operator.or", EnumOperator.OR.getKey());
        assertEquals("OR", EnumOperator.OR.name());
        assertEquals(2, EnumOperator.XOR.ordinal());
        assertEquals("operator.xor", EnumOperator.XOR.getKey());
        assertEquals("XOR", EnumOperator.XOR.name());
    }

    /**
     * Test method for {@link EnumOperator#toString()}.
     */
    @Test
    public void testToString() {
        assertEquals(" AND ", EnumOperator.AND.toString());
        assertEquals(" OR ", EnumOperator.OR.toString());
        assertEquals(" XOR ", EnumOperator.XOR.toString());
    }
}
