/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

/**
 * Check utility class (generic cast).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class CastGenericsTest {

    /**
     * Check cast a object
     */
    @Test
    public void testGetObject() {
        ExException map = new ExException("msg");
        IOException result = CastGenerics.getObject(map, IOException.class);

        assertEquals("msg", result.getMessage());
    }

    /**
     * Check cast list
     */
    @Test
    public void testGetList() {

        List<String> list = new ArrayList<>();
        list.add("value1");
        list.add(null);
        list.add("value2");

        List<String> result = CastGenerics.getList(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));
    }

    /**
     * Check cast hash map
     */
    @Test
    public void testGetHashMap() {

        Map<Object, Object> map = new HashMap<>();
        map.put("key", "value");

        Map<String, String> result = CastGenerics.getHashMap(map, String.class, String.class);
        assertEquals("value", result.get("key"));
    }

    /**
     * Check cast hash set
     */
    @Test
    public void testGetHashSet() {

        Set<Object> set = new HashSet<>();
        set.add("test");

        Set<String> result = CastGenerics.getHashSet(set, String.class);
        assertEquals(1, result.size());
        assertEquals("test", result.toArray(new String[result.size()])[0]);
    }

    /**
     * Check map objects into instanciable
     */
    @Test
    public void testMap() {
        Object[] objects = {"TEST", new Exception("MESSAGE")};

        Exception exception = CastGenerics.map(Exception.class, objects);

        assertEquals(objects[0], exception.getMessage());
        assertEquals(objects[1], exception.getCause());
    }

    static class ExException extends IOException {

        /**
         * serialVersionUID
         */
        private static final long serialVersionUID = 4043557131470608655L;

        /**
         * Constructor
         *
         * @param message
         *            The exception message
         */
        ExException(final String message) {
            super(message);
        }
    }
}
