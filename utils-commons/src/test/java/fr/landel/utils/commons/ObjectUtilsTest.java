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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Check {@link ObjectUtils}
 *
 * @since Aug 15, 2016
 * @author Gilles
 *
 */
public class ObjectUtilsTest {

    /**
     * Test method for {@link ObjectUtils#ObjectUtils()}.
     */
    @Test
    public void testObjectUtils() {
        assertNotNull(new ObjectUtils());
    }

    /**
     * Test method for
     * {@link ObjectUtils#defaultIfNull(java.lang.Object, java.util.function.Supplier)}.
     */
    @Test
    public void testDefaultIfNullTSupplierOfT() {
        assertTrue(ObjectUtils.defaultIfNull(true, () -> false));
        assertFalse(ObjectUtils.defaultIfNull(null, () -> false));
    }

    /**
     * Test method for
     * {@link ObjectUtils#defaultIfNull(java.lang.Object, java.util.function.Supplier)}.
     */
    @Test(expected = NullPointerException.class)
    public void testDefaultIfNullTSupplierOfTNull() {
        ObjectUtils.defaultIfNull(true, null);
    }
}
