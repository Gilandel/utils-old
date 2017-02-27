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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.junit.Test;

/**
 * Check cast utility class: {@link CastUtils}
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public class CastUtilsTest {

    private static final Comparator<String> COMPARATOR = Comparators.STRING.desc();

    /**
     * Check get the class of an object
     */
    @Test
    public void testGetObjectClass() {
        ExException map = new ExException("msg");

        assertNull(CastUtils.getClass(null));
        assertEquals(ExException.class, CastUtils.getClass(map));
    }

    /**
     * Check cast an object
     */
    @Test
    public void testCastObject() {
        ExException map = new ExException("msg");

        assertNull(CastUtils.cast(null));

        try {
            int num = CastUtils.cast((Object) 12);
            assertEquals(12, num);
        } catch (ClassCastException e) {
            fail(e.getMessage());
        }

        try {
            int num = CastUtils.cast(map);
            fail("Map cannot be casted into: " + num);
        } catch (ClassCastException e) {
            assertNotNull(e);
        }
    }

    /**
     * Check cast an object
     */
    @Test
    public void testCastObjectClass() {
        ExException exex = new ExException("msg");
        IOException result = CastUtils.cast(exex, IOException.class);

        assertEquals("msg", result.getMessage());
        assertNull(CastUtils.cast(null, IOException.class));
        assertNull(CastUtils.cast("", IOException.class));
        assertNull(CastUtils.cast("", null));
        assertNull(CastUtils.cast(null, null));
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

        assertTrue(CollectionUtils.isEmpty(CastUtils.getArrayList(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getVector(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getLinkedListAsList(null, String.class)));

        List<String> result = CastUtils.getArrayList(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));

        result = CastUtils.getVector(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));

        result = CastUtils.getLinkedListAsList(list, String.class);
        assertEquals("value1", result.get(0));
        assertNull(result.get(1));
        assertEquals("value2", result.get(2));

        assertEquals(0, CastUtils.getArrayList(list, null).size());
        assertEquals(0, CastUtils.getArrayList(null, null).size());
        assertEquals(0, CastUtils.getArrayList(12, String.class).size());
        assertEquals(0, CastUtils.getArrayList(Arrays.asList(12), String.class).size());
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

        assertTrue(CollectionUtils.isEmpty(CastUtils.getLinkedListAsQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getLinkedTransferQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getPriorityQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getLinkedBlockingQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getPriorityBlockingQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getArrayBlockingQueue(null, String.class, queue.size())));

        Queue<String> result = CastUtils.getLinkedListAsQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertNull(result.poll());
        assertEquals("value2", result.poll());

        result = CastUtils.getLinkedTransferQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastUtils.getPriorityQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastUtils.getLinkedBlockingQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastUtils.getPriorityBlockingQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        result = CastUtils.getArrayBlockingQueue(queue, String.class, queue.size());
        assertEquals("value1", result.poll());
        assertEquals("value2", result.poll());

        assertEquals(0, CastUtils.getLinkedListAsQueue(12, String.class).size());

        Queue<Integer> queue2 = new LinkedList<>();
        queue2.add(2);
        assertEquals(0, CastUtils.getLinkedListAsQueue(queue2, String.class).size());
    }

    /**
     * Check cast hash map
     */
    @Test
    public void testGetMap() {
        Map<Object, Object> map = new HashMap<>();
        map.put("key2", "value");
        map.put("key1", null);
        map.put(null, "value2");

        assertTrue(MapUtils.isEmpty(CastUtils.getHashMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastUtils.getLinkedHashMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastUtils.getHashtable(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastUtils.getTreeMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastUtils.getTreeMap(null, String.class, String.class, COMPARATOR)));

        Map<String, String> result = CastUtils.getHashMap(map, String.class, String.class);
        assertEquals("value", result.get("key2"));
        assertEquals("value2", result.get(null));
        assertNull(result.get("key1"));

        result = CastUtils.getHashtable(map, String.class, String.class);
        assertEquals("value", result.get("key2"));
        assertNull(result.get("key1"));

        result = CastUtils.getTreeMap(map, String.class, String.class, COMPARATOR);
        Iterator<Entry<String, String>> entries = result.entrySet().iterator();
        assertEquals("value", entries.next().getValue());
        assertNull(entries.next().getValue());
        assertEquals("value2", entries.next().getValue());

        // No comparator: TreeMap is not null safe
        result = CastUtils.getTreeMap(map, String.class, String.class);
        assertEquals("value", result.get("key2"));
        assertNull(result.get("key1"));
        // assertEquals("value2", result.get(null));
    }

    /**
     * Check cast hash set
     */
    @Test
    public void testGetSet() {
        Set<Object> set = new HashSet<>();
        set.add("test2");
        set.add(null);
        set.add("test1");

        assertTrue(CollectionUtils.isEmpty(CastUtils.getHashSet(null, null)));

        assertTrue(CollectionUtils.isEmpty(CastUtils.getHashSet(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getTreeSet(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastUtils.getTreeSet(null, String.class, COMPARATOR)));

        assertTrue(CollectionUtils.isEmpty(CastUtils.getHashSet(2, String.class)));

        Set<String> result = CastUtils.getHashSet(set, String.class);
        assertEquals(2, result.size());
        assertTrue(result.contains("test1"));
        assertTrue(result.contains("test2"));

        result = CastUtils.getTreeSet(set, String.class);
        assertEquals(2, result.size());
        Iterator<String> it = result.iterator();
        assertEquals("test1", it.next());
        assertEquals("test2", it.next());

        result = CastUtils.getTreeSet(set, String.class, COMPARATOR);
        assertEquals(2, result.size());
        it = result.iterator();
        assertEquals("test2", it.next());
        assertEquals("test1", it.next());
    }

    /**
     * Check cast iterator
     */
    @Test
    public void testGetIterator() {
        Set<CharSequence> set = new HashSet<>();
        set.add("test");
        set.add(null);

        assertFalse(CastUtils.getIterator(null, String.class).hasNext());
        assertFalse(CastUtils.getIterator(null, null).hasNext());
        assertFalse(CastUtils.getIterator(set.iterator(), null).hasNext());
        assertFalse(CastUtils.getIterator("", null).hasNext());
        assertFalse(CastUtils.getIterator("", String.class).hasNext());
        assertTrue(CastUtils.getIterator(set.iterator(), Integer.class).hasNext());
        assertNull(CastUtils.getIterator(set.iterator(), Integer.class).next());

        Iterator<String> iterator = CastUtils.getIterator(set.iterator(), String.class);
        assertTrue(iterator.hasNext());
        assertNull(iterator.next());
        assertTrue(iterator.hasNext());
        assertEquals("test", iterator.next());
    }

    /**
     * Check map objects into instantiable
     */
    @Test
    public void testMap() {
        Object[] objects = {"TEST", new Exception("MESSAGE")};

        Exception exception = CastUtils.map(Exception.class, objects);

        assertEquals(objects[0], exception.getMessage());
        assertEquals(objects[1], exception.getCause());

        Object[] objects2 = {"TEST", null};

        exception = CastUtils.map(Exception.class, objects2);

        assertEquals(objects2[0], exception.getMessage());
        assertNull(objects2[1]);

        Object[] objects3 = {"TEST"};

        exception = CastUtils.map(Exception.class, objects3);

        assertEquals(objects3[0], exception.getMessage());

        Object[] objects4 = {12};

        assertNull(CastUtils.map(Exception.class, objects4));

        Object[] objects5 = {"TEST", -1};

        assertNull(CastUtils.map(ExException.class, objects5));
        assertNull(CastUtils.map(Exception.class, "", ""));

        assertNull(CastUtils.map(null, objects4));
        assertNull(CastUtils.map(null, (Object[]) null));
        assertNull(CastUtils.map(Exception.class, (Object[]) null));
    }

    /**
     * Check get typed list class
     */
    @Test
    public void testGetTypedListClass() {
        Class<List<Exception>> clazz = CastUtils.getTypedListClass(Exception.class);
        assertNotNull(clazz);
        assertNotNull(CastUtils.getTypedListClass(List.class));
        assertNotNull(CastUtils.getTypedListClass(null));
        assertTrue(List.class.isAssignableFrom(clazz));
    }

    /**
     * Check get typed set class
     */
    @Test
    public void testGetTypedSetClass() {
        Class<Set<Exception>> clazz = CastUtils.getTypedSetClass(Exception.class);
        assertNotNull(clazz);
        assertNotNull(CastUtils.getTypedSetClass(Exception.class));
        assertNotNull(CastUtils.getTypedSetClass(List.class));
        assertNotNull(CastUtils.getTypedSetClass(null));
        assertTrue(Set.class.isAssignableFrom(clazz));
    }

    /**
     * Check get typed queue class
     */
    @Test
    public void testGetTypedQueueClass() {
        Class<Queue<Exception>> clazz = CastUtils.getTypedQueueClass(Exception.class);
        assertNotNull(clazz);
        assertNotNull(CastUtils.getTypedQueueClass(List.class));
        assertNotNull(CastUtils.getTypedQueueClass(null));
        assertTrue(Queue.class.isAssignableFrom(clazz));
    }

    /**
     * Check get typed map class
     */
    @Test
    public void testGetTypedMapClass() {
        Class<Map<Exception, Integer>> clazz = CastUtils.getTypedMapClass(Exception.class, Integer.class);
        assertNotNull(clazz);
        assertNotNull(CastUtils.getTypedMapClass(List.class, Integer.class));
        assertNotNull(CastUtils.getTypedMapClass(String.class, String.class));
        assertNotNull(CastUtils.getTypedMapClass(null, null));
        assertTrue(Map.class.isAssignableFrom(clazz));
    }

    /**
     * Check {@link CastUtils#getConstructor}
     */
    @Test
    public void testGetConstructor() {
        Constructor<ExException> constructor = CastUtils.getConstructor(ExException.class, String.class, Integer.class);
        assertNotNull(constructor);
        assertNotNull(CastUtils.instantiate(false, constructor, "message", 1));
        assertNull(CastUtils.instantiate(false, constructor, "message", "Test"));
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

        /**
         * Constructor
         *
         * @param message
         *            The exception message
         * @param value
         *            The value
         */
        ExException(final String message, final Integer value) {
            super(message);

            if (value < 0) {
                throw new IllegalArgumentException("Value cannot be negative");
            }
        }

        /**
         * Constructor
         *
         * @param message
         *            The exception message
         * @param description
         *            The description
         */
        ExException(final String message, final String description) {
            super(message);

            if (description == null) {
                throw new IllegalArgumentException("description cannot be null");
            }
        }
    }
}
