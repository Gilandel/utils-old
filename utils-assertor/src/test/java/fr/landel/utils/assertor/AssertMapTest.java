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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import fr.landel.utils.assertor.AssertMap;
import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.assertor.Expect;

/**
 * Check {@link AssertMap}
 *
 * @since 5 juin 2016
 * @author Gilles
 *
 */
public class AssertMapTest {

    private final String ERROR = "error expected";

    /**
     * Test method for {@link AssertMap#isEmpty}.
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testIsEmpty() throws IOException {
        final String el = "element";

        final Map<String, Integer> map = new HashMap<>();

        final AssertMap<String, Integer> assertMap = Assertor.that(map);

        assertMap.isEmpty().toThrow();

        map.put(el, 1);

        Expect.exception(() -> {
            assertMap.isEmpty().toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            assertMap.isEmpty().toThrow("map is not empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] map is not empty");

        Expect.exception(() -> {
            assertMap.isEmpty().toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        Assertor.that((Map<String, Integer>) null).isEmpty().toThrow("this argument is required; it must not be null");
    }

    /**
     * Test method for {@link AssertMap#isNotEmpty}.
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testIsNotEmpty() throws IOException {
        final String el = "element";

        final Map<String, Integer> map = new HashMap<>();
        map.put(el, 1);

        final AssertMap<String, Integer> assertMap = Assertor.that(map);

        assertMap.isNotEmpty().toThrow();

        map.clear();

        Expect.exception(() -> {
            assertMap.isNotEmpty().toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            assertMap.isNotEmpty().toThrow("map is empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] map is empty");

        Expect.exception(() -> {
            assertMap.isNotEmpty().toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        assertFalse(Assertor.that((Map<String, Integer>) null).isNotEmpty().getResult());
    }

    /**
     * Test method for {@link AssertMap#contains}.
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testContains() throws IOException {
        final String key1 = "element1";
        final String key2 = "element2";
        final Integer val1 = 1;

        final Map<String, Integer> map = new HashMap<>();
        map.put(key1, val1);
        map.put(key2, null);

        final AssertMap<String, Integer> assertMap = Assertor.that(map);

        assertMap.isNotNull().and().contains(key1).toThrow();

        final List<String> keys = Arrays.asList(key1);

        assertMap.contains(key1).toThrow();
        assertMap.contains(key1).toThrow("map doesn't contain the element %2$p");
        assertMap.contains(key1).toThrow(new IOException());
        assertMap.contains(key1, val1).toThrow();
        assertMap.contains(key1, val1).toThrow("map doesn't contain the element %3$p");
        assertMap.contains(key1, val1).toThrow(new IOException());
        assertMap.contains(key2, null).toThrow();
        assertMap.contains(keys).toThrow();
        assertMap.contains(keys).toThrow("map doesn't contain the element %2$p");
        assertMap.contains(keys).toThrow(new IOException());
        assertMap.contains(map).toThrow();
        assertMap.contains(map).toThrow("map doesn't contain the element %2$p");
        assertMap.contains(map).toThrow(new IOException());

        Map<String, Integer> map1 = new HashMap<>();
        map1.put("element3", 2);

        assertFalse(Assertor.that(map).contains(key1, (Integer) null).getResult());
        assertTrue(Assertor.that(map).contains(key2, (Integer) null).getResult());
        assertFalse(Assertor.that(map).contains(key2, 1).getResult());
        assertFalse(Assertor.that(map).contains(Arrays.asList("element3")).getResult());
        assertFalse(Assertor.that(map).contains(map1).getResult());

        assertFalse(Assertor.that(map).contains((String) null).getResult());
        assertFalse(Assertor.that(map).contains((String) null, (Integer) null).getResult());
        assertFalse(Assertor.that(map).contains((List<String>) null).getResult());
        assertFalse(Assertor.that(map).contains((Map<String, Integer>) null).getResult());

        assertFalse(Assertor.that((Map<String, Integer>) null).contains(key1).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).contains(key1, val1).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).contains(keys).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).contains(map).getResult());
    }

    /**
     * Test method for {@link AssertMap#doesNotContain}.
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testDoesNotContain() throws IOException {
        final String key1 = "element1";
        final Integer val1 = 1;
        final String key2 = "element2";
        final Integer val2 = 2;

        final Map<String, Integer> map = new HashMap<>();
        map.put(key1, val1);

        final AssertMap<String, Integer> assertMap = Assertor.that(map);

        final Map<String, Integer> map1 = new HashMap<>();
        map1.put("element3", 2);

        final List<String> keys = Arrays.asList("element3");

        assertMap.isNotNull().and().contains(key1).toThrow();

        assertMap.doesNotContain(key2).toThrow();
        assertMap.doesNotContain(key2).toThrow("map contains the element %2$p");
        assertMap.doesNotContain(key2).toThrow(new IOException());
        assertMap.doesNotContain(key2, val2).toThrow();
        assertMap.doesNotContain(key2, val2).toThrow("map contains the element %3$p");
        assertMap.doesNotContain(key2, val2).toThrow(new IOException());
        assertMap.doesNotContain(keys).toThrow();
        assertMap.doesNotContain(keys).toThrow("map doesn't contain the element %2$p");
        assertMap.doesNotContain(keys).toThrow(new IOException());
        assertMap.doesNotContain(map1).toThrow();
        assertMap.doesNotContain(map1).toThrow("map doesn't contain the element %2$p");
        assertMap.doesNotContain(map1).toThrow(new IOException());

        assertFalse(Assertor.that(map).doesNotContain(key1).getResult());
        assertFalse(Assertor.that(map).doesNotContain(key1, val1).getResult());
        assertFalse(Assertor.that(map).doesNotContain(map.keySet()).getResult());
        assertFalse(Assertor.that(map).doesNotContain(map).getResult());

        assertTrue(Assertor.that(map).doesNotContain(key1, (Integer) null).getResult());
        assertTrue(Assertor.that(map).doesNotContain(Arrays.asList("element3")).getResult());
        assertTrue(Assertor.that(map).doesNotContain(map1).getResult());

        assertTrue(Assertor.that(map).doesNotContain((String) null).getResult());
        assertTrue(Assertor.that(map).doesNotContain(key1, 3).getResult());
        assertTrue(Assertor.that(map).doesNotContain((String) null, (Integer) null).getResult());
        assertFalse(Assertor.that(map).doesNotContain((List<String>) null).getResult());
        assertFalse(Assertor.that(map).doesNotContain((Map<String, Integer>) null).getResult());

        assertFalse(Assertor.that((Map<String, Integer>) null).doesNotContain(key1).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).doesNotContain(key1, val1).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).doesNotContain(keys).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).doesNotContain(map1).getResult());
    }

    /**
     * Test method for {@link AssertMap#isNotEmpty(Map, String, Object...)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQString() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("f", "f");
            Assertor.that(map).isNotEmpty().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertMap#isNotEmpty(Map, String, Object...)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQString() {
        Assertor.that(new HashMap<String, String>()).isNotEmpty().toThrow();
    }

    /**
     * Test method for {@link AssertMap#isNotEmpty(java.util.Map)} .
     */
    @Test
    public void testIsNotEmptyOKMapOfQQ() {
        try {
            Map<String, String> map = new HashMap<>();
            map.put("fg", "fg");
            Assertor.that(map).isNotEmpty().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertMap#isNotEmpty(java.util.Map)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOMapOfQQ() {
        Assertor.that(new HashMap<String, String>()).isNotEmpty().toThrow();
    }

    /**
     * Test method for {@link AssertMap#hasSize}.
     */
    @Test
    public void testHasSize() {
        final String key1 = "element1";
        final Integer val1 = 1;

        final Map<String, Integer> map = new HashMap<>();
        map.put(key1, val1);

        assertTrue(Assertor.that(map).hasSize(1).getResult());
        assertFalse(Assertor.that(map).hasSize(0).getResult());
        assertFalse(Assertor.that(map).hasSize(2).getResult());

        assertFalse(Assertor.that(map).hasSize(-1).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).hasSize(1).getResult());
    }

    /**
     * Test method for {@link AssertMap#hasNotSize}.
     */
    @Test
    public void testHasNotSize() {
        final String key1 = "element1";
        final Integer val1 = 1;

        final Map<String, Integer> map = new HashMap<>();
        map.put(key1, val1);

        assertFalse(Assertor.that(map).hasNotSize(1).getResult());
        assertTrue(Assertor.that(map).hasNotSize(0).getResult());
        assertTrue(Assertor.that(map).hasNotSize(2).getResult());

        assertFalse(Assertor.that(map).hasNotSize(-1).getResult());
        assertFalse(Assertor.that((Map<String, Integer>) null).hasNotSize(1).getResult());
    }
}
