/*
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.Test;

/**
 * Check utility class.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public class ClassUtilsTest {

    /**
     * Test method for {@link ClassUtils#getSuperclasses(java.lang.Class)} .
     */
    @Test
    public void testGetSuperclasses() {
        List<Class<?>> expectedClasses = Arrays.asList(TreeMap.class, AbstractMap.class, NavigableMap.class, Cloneable.class,
                Serializable.class, Map.class, SortedMap.class, Object.class);

        Set<Class<?>> treeMapClasses = ClassUtils.getSuperclasses(TreeMap.class);

        assertNotNull(treeMapClasses);
        assertEquals(expectedClasses.size(), treeMapClasses.size());

        for (Class<?> treeMapClass : treeMapClasses) {
            if (!expectedClasses.contains(treeMapClass)) {
                fail("Super class not found: " + treeMapClass);
            }
        }

        // Interface

        expectedClasses = Arrays.asList(Map.class, Object.class);

        treeMapClasses = ClassUtils.getSuperclasses(Map.class);

        assertNotNull(treeMapClasses);
        assertEquals(expectedClasses.size(), treeMapClasses.size());

        for (Class<?> treeMapClass : treeMapClasses) {
            if (!expectedClasses.contains(treeMapClass)) {
                fail("Super class not found: " + treeMapClass);
            }
        }
    }

    /**
     * Test method for
     * {@link ClassUtils#getCommonSuperclasses(java.lang.Class, java.lang.Class)}
     * .
     */
    @Test
    public void testGetCommonSuperclasses() {
        final List<Class<?>> expectedClasses = Arrays.asList(AbstractMap.class, Cloneable.class, Serializable.class, Map.class,
                Object.class);

        final Set<Class<?>> mapClasses = ClassUtils.getCommonSuperclasses(TreeMap.class, HashMap.class);

        assertNotNull(mapClasses);
        assertEquals(expectedClasses.size(), mapClasses.size());

        for (Class<?> mapClass : mapClasses) {
            if (!expectedClasses.contains(mapClass)) {
                fail("Common super class not found: " + mapClass);
            }
        }
    }

    /**
     * Test method for {@link ClassUtils#isAnyNull(Object...)} .
     */
    @Test
    public void testIsAnyNull() {
        assertFalse(ClassUtils.isAnyNull());
        assertTrue(ClassUtils.isAnyNull((Object) null));
        assertFalse(ClassUtils.isAnyNull((Object[]) null));
        assertFalse(ClassUtils.isAnyNull(new Object[0]));
        assertTrue(ClassUtils.isAnyNull(null, ClassUtils.class));
        assertFalse(ClassUtils.isAnyNull(ClassUtilsTest.class, ClassUtils.class));
        assertTrue(ClassUtils.isAnyNull(ClassUtilsTest.class, ClassUtils.class, null));
    }

    /**
     * Test method for {@link ClassUtils#isAllNull(Object...)} .
     */
    @Test
    public void testIsAllNull() {
        assertFalse(ClassUtils.isAllNull());
        assertTrue(ClassUtils.isAllNull((Object) null));
        assertFalse(ClassUtils.isAllNull((Object[]) null));
        assertFalse(ClassUtils.isAllNull(new Object[0]));
        assertFalse(ClassUtils.isAllNull(null, ClassUtils.class));
        assertFalse(ClassUtils.isAllNull(ClassUtilsTest.class, ClassUtils.class));
        assertFalse(ClassUtils.isAllNull(ClassUtilsTest.class, ClassUtils.class, null));
        assertTrue(ClassUtils.isAllNull(null, null, null));
    }

    /**
     * Test method for {@link ClassUtils#isNoneNull(Object...)} .
     */
    @Test
    public void testIsNoneNull() {
        assertFalse(ClassUtils.isNoneNull());
        assertFalse(ClassUtils.isNoneNull((Object) null));
        assertFalse(ClassUtils.isNoneNull((Object[]) null));
        assertFalse(ClassUtils.isNoneNull(new Object[0]));
        assertTrue(ClassUtils.isNoneNull(ClassUtilsTest.class));
        assertFalse(ClassUtils.isNoneNull(null, ClassUtils.class));
        assertTrue(ClassUtils.isNoneNull(ClassUtilsTest.class, ClassUtils.class));
        assertFalse(ClassUtils.isNoneNull(ClassUtilsTest.class, ClassUtils.class, null));
        assertFalse(ClassUtils.isNoneNull(null, null, null));
    }

    /**
     * Test method for {@link ClassUtils#getName(Object)} .
     */
    @Test
    public void testGetName() {
        assertEquals("null", ClassUtils.getName(null));
        assertEquals("java.lang.Integer", ClassUtils.getName(3));
        assertEquals("java.lang.Integer", ClassUtils.getCanonicalName(3));
        assertEquals("Integer", ClassUtils.getSimpleName(3));
        assertEquals("[Ljava.lang.Integer;", ClassUtils.getName(new Integer[] {3}));
        assertEquals("java.lang.Integer[]", ClassUtils.getTypeName(new Integer[] {3}));
        assertEquals("java.lang", ClassUtils.getPackageName(3));
    }

    /**
     * Test method for {@link ClassUtils#isAssignableFrom(Class, Object)} .
     */
    @Test
    public void testIsAssignableFrom() {
        assertTrue(ClassUtils.isAssignableFrom(String.class, ""));
        assertTrue(ClassUtils.isAssignableFrom(CharSequence.class, ""));
        assertFalse(ClassUtils.isAssignableFrom(String.class, 12));
        assertFalse(ClassUtils.isAssignableFrom(String.class, null));

        assertFalse(ClassUtils.isAssignableFrom(null, ""));
        assertFalse(ClassUtils.isAssignableFrom(null, 12));
        assertFalse(ClassUtils.isAssignableFrom(null, null));
    }
}
