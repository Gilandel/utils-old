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
    public void testDefaultIfNullTOfT() {
        assertTrue(ObjectUtils.defaultIfNull(true, () -> false));
        assertFalse(ObjectUtils.defaultIfNull(null, () -> false));
    }

    /**
     * Test method for
     * {@link ObjectUtils#defaultIfNull(Object, Object, java.util.function.Function)}.
     */
    @Test
    public void testDefaultIfNullTOfT2() {
        assertTrue(ObjectUtils.defaultIfNull(false, false, b -> !b));
        assertFalse(ObjectUtils.defaultIfNull((Boolean) null, false, b -> !b));
    }

    /**
     * Test method for
     * {@link ObjectUtils#defaultIfSupplyNull(java.util.function.Supplier, Object)}.
     */
    @Test(expected = NullPointerException.class)
    public void testDefaultIfNullSupplierNull() {
        ObjectUtils.defaultIfNull(null, true, null);
    }

    /**
     * Test method for
     * {@link ObjectUtils#defaultIfSupplyNull(java.util.function.Supplier, java.util.function.Supplier)}.
     */
    @Test(expected = NullPointerException.class)
    public void testDefaultIfNullDefaultNull1() {
        ObjectUtils.defaultIfNull(null, null);
    }

    /**
     * Test method for
     * {@link ObjectUtils#defaultIfSupplyNull(java.lang.Object, java.util.function.Supplier)}.
     */
    @Test(expected = NullPointerException.class)
    public void testDefaultIfNullTSupplierOfTNull() {
        ObjectUtils.defaultIfNull(true, null);
    }

    /**
     * Test method for {@link ObjectUtils#allNull(java.lang.Object...)}.
     */
    @Test
    public void testAllNull() {
        assertFalse(ObjectUtils.allNull(15));
        assertFalse(ObjectUtils.allNull(15, null));
        assertTrue(ObjectUtils.allNull(null, null));
        assertTrue(ObjectUtils.allNull((Object) null));
    }

    /**
     * Test method for {@link ObjectUtils#allNull(java.lang.Object...)}.
     */
    @Test(expected = NullPointerException.class)
    public void testAllNullKO() {
        ObjectUtils.allNull((Object[]) null);
    }

    /**
     * Test method for {@link ObjectUtils#anyNull(java.lang.Object...)}.
     */
    @Test
    public void testAnyNull() {
        assertFalse(ObjectUtils.anyNull(15));
        assertTrue(ObjectUtils.anyNull(15, null));
        assertTrue(ObjectUtils.anyNull(null, null));
        assertTrue(ObjectUtils.anyNull((Object) null));
    }

    /**
     * Test method for {@link ObjectUtils#anyNull(java.lang.Object...)}.
     */
    @Test(expected = NullPointerException.class)
    public void testAnyNullKO() {
        ObjectUtils.anyNull((Object[]) null);
    }

    /**
     * Test method for {@link ObjectUtils#allNull(java.lang.Object...)}.
     */
    @Test
    public void testAllNotNull() {
        assertTrue(ObjectUtils.allNotNull(15));
        assertFalse(ObjectUtils.allNotNull(15, null));
        assertFalse(ObjectUtils.allNotNull(null, null));
        assertFalse(ObjectUtils.allNotNull((Object) null));
    }

    /**
     * Test method for {@link ObjectUtils#allNotNull(java.lang.Object...)}.
     */
    @Test(expected = NullPointerException.class)
    public void testAllNotNullKO() {
        ObjectUtils.allNotNull((Object[]) null);
    }

    /**
     * Test method for {@link ObjectUtils#anyNotNull(java.lang.Object...)}.
     */
    @Test
    public void testAnyNotNull() {
        assertTrue(ObjectUtils.anyNotNull(15));
        assertTrue(ObjectUtils.anyNotNull(15, null));
        assertFalse(ObjectUtils.anyNotNull(null, null));
        assertFalse(ObjectUtils.anyNotNull((Object) null));
    }

    /**
     * Test method for {@link ObjectUtils#anyNull(java.lang.Object...)}.
     */
    @Test(expected = NullPointerException.class)
    public void testAnyNotNullKO() {
        ObjectUtils.anyNotNull((Object[]) null);
    }
}
