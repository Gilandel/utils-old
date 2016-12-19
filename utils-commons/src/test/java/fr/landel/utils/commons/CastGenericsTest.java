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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

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
 * Check utility class (generic cast).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class CastGenericsTest {

    private static final Comparator<String> COMPARATOR = Comparators.STRING.desc();

    /**
     * Check cast a object
     */
    @Test
    public void testGetObjectClass() {
        ExException map = new ExException("msg");

        assertNull(CastGenerics.getClass(null));
        assertEquals(ExException.class, CastGenerics.getClass(map));
    }

    /**
     * Check cast a object
     */
    @Test
    public void testGetObject() {
        ExException exex = new ExException("msg");
        IOException result = CastGenerics.cast(exex, IOException.class);

        assertEquals("msg", result.getMessage());
        assertNull(CastGenerics.cast(null, IOException.class));
        assertNull(CastGenerics.cast("", IOException.class));
        assertNull(CastGenerics.cast("", null));
        assertNull(CastGenerics.cast(null, null));
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

        assertEquals(0, CastGenerics.getArrayList(list, null).size());
        assertEquals(0, CastGenerics.getArrayList(null, null).size());
        assertEquals(0, CastGenerics.getArrayList(12, String.class).size());
        assertEquals(0, CastGenerics.getArrayList(Arrays.asList(12), String.class).size());
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
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getLinkedTransferQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getPriorityQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getLinkedBlockingQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getPriorityBlockingQueue(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getArrayBlockingQueue(null, String.class, queue.size())));

        Queue<String> result = CastGenerics.getLinkedListAsQueue(queue, String.class);
        assertEquals("value1", result.poll());
        assertNull(result.poll());
        assertEquals("value2", result.poll());

        result = CastGenerics.getLinkedTransferQueue(queue, String.class);
        assertEquals("value1", result.poll());
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

        assertEquals(0, CastGenerics.getLinkedListAsQueue(12, String.class).size());

        Queue<Integer> queue2 = new LinkedList<>();
        queue2.add(2);
        assertEquals(0, CastGenerics.getLinkedListAsQueue(queue2, String.class).size());
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

        assertTrue(MapUtils.isEmpty(CastGenerics.getHashMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getLinkedHashMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getHashtable(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getTreeMap(null, String.class, String.class)));
        assertTrue(MapUtils.isEmpty(CastGenerics.getTreeMap(null, String.class, String.class, COMPARATOR)));

        Map<String, String> result = CastGenerics.getHashMap(map, String.class, String.class);
        assertEquals("value", result.get("key2"));
        assertEquals("value2", result.get(null));
        assertNull(result.get("key1"));

        result = CastGenerics.getHashtable(map, String.class, String.class);
        assertEquals("value", result.get("key2"));
        assertNull(result.get("key1"));

        result = CastGenerics.getTreeMap(map, String.class, String.class, COMPARATOR);
        Iterator<Entry<String, String>> entries = result.entrySet().iterator();
        assertEquals("value", entries.next().getValue());
        assertNull(entries.next().getValue());
        assertEquals("value2", entries.next().getValue());

        // No comparator: TreeMap is not null safe
        result = CastGenerics.getTreeMap(map, String.class, String.class);
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

        assertTrue(CollectionUtils.isEmpty(CastGenerics.getHashSet(null, null)));

        assertTrue(CollectionUtils.isEmpty(CastGenerics.getHashSet(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getTreeSet(null, String.class)));
        assertTrue(CollectionUtils.isEmpty(CastGenerics.getTreeSet(null, String.class, COMPARATOR)));

        assertTrue(CollectionUtils.isEmpty(CastGenerics.getHashSet(2, String.class)));

        Set<String> result = CastGenerics.getHashSet(set, String.class);
        assertEquals(2, result.size());
        assertTrue(result.contains("test1"));
        assertTrue(result.contains("test2"));

        result = CastGenerics.getTreeSet(set, String.class);
        assertEquals(2, result.size());
        Iterator<String> it = result.iterator();
        assertEquals("test1", it.next());
        assertEquals("test2", it.next());

        result = CastGenerics.getTreeSet(set, String.class, COMPARATOR);
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

        assertFalse(CastGenerics.getIterator(null, String.class).hasNext());
        assertFalse(CastGenerics.getIterator(null, null).hasNext());
        assertFalse(CastGenerics.getIterator(set.iterator(), null).hasNext());
        assertFalse(CastGenerics.getIterator("", null).hasNext());
        assertFalse(CastGenerics.getIterator("", String.class).hasNext());
        assertTrue(CastGenerics.getIterator(set.iterator(), Integer.class).hasNext());
        assertNull(CastGenerics.getIterator(set.iterator(), Integer.class).next());

        Iterator<String> iterator = CastGenerics.getIterator(set.iterator(), String.class);
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

        Exception exception = CastGenerics.map(Exception.class, objects);

        assertEquals(objects[0], exception.getMessage());
        assertEquals(objects[1], exception.getCause());

        Object[] objects2 = {"TEST", null};

        exception = CastGenerics.map(Exception.class, objects2);

        assertEquals(objects2[0], exception.getMessage());
        assertNull(objects2[1]);

        Object[] objects3 = {"TEST"};

        exception = CastGenerics.map(Exception.class, objects3);

        assertEquals(objects3[0], exception.getMessage());

        Object[] objects4 = {12};

        assertNull(CastGenerics.map(Exception.class, objects4));

        Object[] objects5 = {"TEST", -1};

        assertNull(CastGenerics.map(ExException.class, objects5));
        assertNull(CastGenerics.map(Exception.class, "", ""));

        assertNull(CastGenerics.map(null, objects4));
        assertNull(CastGenerics.map(null, (Object[]) null));
        assertNull(CastGenerics.map(Exception.class, (Object[]) null));
    }

    /**
     * Check get typed list class
     */
    @Test
    public void testGetTypedListClass() {
        Class<List<Exception>> clazz = CastGenerics.getTypedListClass(Exception.class);
        assertNotNull(clazz);
        assertNotNull(CastGenerics.getTypedListClass(List.class));
        assertNotNull(CastGenerics.getTypedListClass(null));
        assertTrue(List.class.isAssignableFrom(clazz));
    }

    /**
     * Check get typed set class
     */
    @Test
    public void testGetTypedSetClass() {
        Class<Set<Exception>> clazz = CastGenerics.getTypedSetClass(Exception.class);
        assertNotNull(clazz);
        assertNotNull(CastGenerics.getTypedSetClass(Exception.class));
        assertNotNull(CastGenerics.getTypedSetClass(List.class));
        assertNotNull(CastGenerics.getTypedSetClass(null));
        assertTrue(Set.class.isAssignableFrom(clazz));
    }

    /**
     * Check get typed queue class
     */
    @Test
    public void testGetTypedQueueClass() {
        Class<Queue<Exception>> clazz = CastGenerics.getTypedQueueClass(Exception.class);
        assertNotNull(clazz);
        assertNotNull(CastGenerics.getTypedQueueClass(List.class));
        assertNotNull(CastGenerics.getTypedQueueClass(null));
        assertTrue(Queue.class.isAssignableFrom(clazz));
    }

    /**
     * Check get typed map class
     */
    @Test
    public void testGetTypedMapClass() {
        Class<Map<Exception, Integer>> clazz = CastGenerics.getTypedMapClass(Exception.class, Integer.class);
        assertNotNull(clazz);
        assertNotNull(CastGenerics.getTypedMapClass(List.class, Integer.class));
        assertNotNull(CastGenerics.getTypedMapClass(String.class, String.class));
        assertNotNull(CastGenerics.getTypedMapClass(null, null));
        assertTrue(Map.class.isAssignableFrom(clazz));
    }

    /**
     * Check {@link CastGenerics#getConstructor}
     */
    @Test
    public void testGetConstructor() {
        Constructor<ExException> constructor = CastGenerics.getConstructor(ExException.class, String.class, Integer.class);
        assertNotNull(constructor);
        assertNotNull(CastGenerics.instantiate(false, constructor, "message", 1));
        assertNull(CastGenerics.instantiate(false, constructor, "message", "Test"));
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
