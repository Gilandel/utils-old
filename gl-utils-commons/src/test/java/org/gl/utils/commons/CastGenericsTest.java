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
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
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
    public void testGetObjectClass() {
        ExException map = new ExException("msg");

        assertNull(CastGenerics.getObjectClass(null));
        assertEquals(ExException.class, CastGenerics.getObjectClass(map));
    }

    /**
     * Check cast a object
     */
    @Test
    public void testGetObject() {
        ExException map = new ExException("msg");
        IOException result = CastGenerics.getObject(map, IOException.class);

        assertEquals("msg", result.getMessage());
        assertNull(CastGenerics.getObject(null, IOException.class));
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

        assertTrue(CollectionUtils.isEmpty(CastGenerics.getArrayList(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getVector(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getLinkedListAsList(null, String.class)));

        List<String> result = CastGenerics.getArrayList(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));

        result = CastGenerics.getVector(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));

        result = CastGenerics.getLinkedListAsList(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));
    }

    /**
     * Check cast list
     */
    @Test
    public void testGetQueue() {

        Queue<String> queue = new LinkedList<>();
        queue.add("value1");
        queue.add(null);
        queue.add("value2");

        assertTrue(CollectionUtils.isEmpty(CastGenerics.getLinkedListAsQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getPriorityQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getLinkedBlockingQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getPriorityBlockingQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getArrayBlockingQueue(null, String.class, queue.size())));

        Queue<String> result = CastGenerics.getLinkedListAsQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertNull(result.poll());
        assertEquals("value2", result.poll());

        result = CastGenerics.getPriorityQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastGenerics.getLinkedBlockingQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastGenerics.getPriorityBlockingQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastGenerics.getArrayBlockingQueue(queue, String.class, queue.size());
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());
    }

    /**
     * Check cast hash map
     */
    @Test
    public void testGetMap() {
        final Comparator<String> comparator = new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.compareTo(o2);
            }
        };

        Map<Object, Object> map = new HashMap<>();
        map.put("key", "value");

        assertTrue(MapUtils.isEmpty(CastGenerics.getHashMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getHashtable(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getTreeMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getTreeMap(null, String.class, String.class, comparator)));

        Map<String, String> result = CastGenerics.getHashMap(map, String.class, String.class);
        assertEquals("value", result.get("key"));

        result = CastGenerics.getHashtable(map, String.class, String.class);
        assertEquals("value", result.get("key"));

        result = CastGenerics.getTreeMap(map, String.class, String.class);
        assertEquals("value", result.get("key"));

        result = CastGenerics.getTreeMap(map, String.class, String.class, comparator);
        assertEquals("value", result.get("key"));
    }

    /**
     * Check cast hash set
     */
    @Test
    public void testGetHashSet() {
        final Comparator<String> comparator = new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.compareTo(o2);
            }
        };

        Set<Object> set = new HashSet<>();
        set.add("test");

        assertTrue(CollectionUtils.isEmpty(CastGenerics.getHashSet(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getTreeSet(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getTreeSet(null, String.class, comparator)));

        Set<String> result = CastGenerics.getHashSet(set, String.class);
        assertEquals(1, result.size());
        assertEquals("test", result.toArray(new String[result.size()])[0]);

        result = CastGenerics.getTreeSet(set, String.class);
        assertEquals(1, result.size());
        assertEquals("test", result.toArray(new String[result.size()])[0]);

        result = CastGenerics.getTreeSet(set, String.class, comparator);
        assertEquals(1, result.size());
        assertEquals("test", result.toArray(new String[result.size()])[0]);
    }

    /**
     * Check map objects into instantiable
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
