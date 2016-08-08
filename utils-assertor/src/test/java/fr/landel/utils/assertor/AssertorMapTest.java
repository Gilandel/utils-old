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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check {@link AssertMap}
 *
 * @since 5 juin 2016
 * @author Gilles
 *
 */
public class AssertorMapTest extends AbstractTest {

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

        final PredicateAssertorMap<String, Integer> assertMap = Assertor.that(map);

        assertMap.isEmpty().toThrow();

        map.put(el, 1);

        Expect.exception(() -> {
            assertMap.isEmpty().toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            assertMap.isEmpty().toThrow("map is not empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "map is not empty");

        Expect.exception(() -> {
            assertMap.isEmpty().toThrow(new IOException(), true);
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

        final PredicateAssertorMap<String, Integer> assertMap = Assertor.that(map);

        assertMap.isNotEmpty().toThrow();

        map.clear();

        Expect.exception(() -> {
            assertMap.isNotEmpty().toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            assertMap.isNotEmpty().toThrow("map is empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "map is empty");

        Expect.exception(() -> {
            assertMap.isNotEmpty().toThrow(new IOException(), true);
            fail(ERROR);
        }, IOException.class);

        assertFalse(Assertor.that((Map<String, Integer>) null).isNotEmpty().isOK());
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

        final PredicateAssertorMap<String, Integer> assertMap = Assertor.that(map);

        assertMap.isNotNull().and().contains(key1).toThrow();

        final List<String> keys = Arrays.asList(key1);

        assertMap.contains(key1).toThrow();
        assertMap.contains(key1).toThrow("map doesn't contain the element %2$s*");
        assertMap.contains(key1).toThrow(new IOException(), true);
        assertMap.contains(key1, val1).toThrow();
        assertMap.contains(key1, val1).toThrow("map doesn't contain the element %3$s*");
        assertMap.contains(key1, val1).toThrow(new IOException(), true);
        assertMap.contains(key2, null).toThrow();

        assertMap.containsAll(keys).toThrow();
        assertMap.containsAll(keys).toThrow("map doesn't contain the element %2$s*");
        assertMap.containsAll(keys).toThrow(new IOException(), true);
        assertMap.containsAll(map).toThrow();
        assertMap.containsAll(map).toThrow("map doesn't contain the element %2$s*");
        assertMap.containsAll(map).toThrow(new IOException(), true);

        assertMap.containsAny(keys).toThrow();
        assertMap.containsAny(keys).toThrow("map doesn't contain the element %2$s*");
        assertMap.containsAny(keys).toThrow(new IOException(), true);
        assertMap.containsAny(map).toThrow();
        assertMap.containsAny(map).toThrow("map doesn't contain the element %2$s*");
        assertMap.containsAny(map).toThrow(new IOException(), true);

        assertTrue(assertMap.contains(key1).and().isNotEmpty().isOK());
        assertTrue(assertMap.contains(key1).or().isEmpty().isOK());
        assertTrue(assertMap.contains(key1).xor().isEmpty().isOK());

        Map<String, Integer> map1 = new HashMap<>();
        map1.put("element3", 2);

        assertFalse(Assertor.that(map).contains(key1, (Integer) null).isOK());
        assertTrue(Assertor.that(map).contains(key2, (Integer) null).isOK());
        assertFalse(Assertor.that(map).contains(key2, 1).isOK());
        assertFalse(Assertor.that(map).containsAll(Arrays.asList("element3")).isOK());
        assertFalse(Assertor.that(map).containsAll(map1).isOK());
        assertFalse(Assertor.that(map).containsAny(Arrays.asList("element3")).isOK());
        assertFalse(Assertor.that(map).containsAny(map1).isOK());

        assertFalse(Assertor.that(map).contains((String) null).isOK());
        assertFalse(Assertor.that(map).contains((String) null, (Integer) null).isOK());
        assertFalse(Assertor.that(map).containsAll((List<String>) null).isOK());
        assertFalse(Assertor.that(map).containsAll((Map<String, Integer>) null).isOK());
        assertFalse(Assertor.that(map).containsAny((List<String>) null).isOK());
        assertFalse(Assertor.that(map).containsAny((Map<String, Integer>) null).isOK());

        assertFalse(Assertor.that((Map<String, Integer>) null).contains(key1).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).contains(key1, val1).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).containsAll(keys).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).containsAll(map).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).containsAny(keys).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).containsAny(map).isOK());
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

        final PredicateAssertorMap<String, Integer> assertMap = Assertor.that(map);

        final Map<String, Integer> map1 = new HashMap<>();
        map1.put("element3", 2);

        final List<String> keys = Arrays.asList("element3");

        assertMap.isNotNull().and().contains(key1).toThrow();

        assertMap.not().contains(key2).toThrow();
        assertMap.not().contains(key2).toThrow("map contains the element %2$s*");
        assertMap.not().contains(key2).toThrow(new IOException(), true);
        assertMap.not().contains(key2, val2).toThrow();
        assertMap.not().contains(key2, val2).toThrow("map contains the element %3$s*");
        assertMap.not().contains(key2, val2).toThrow(new IOException(), true);

        assertFalse(assertMap.not().containsAll(keys).isOK());
        assertFalse(assertMap.not().containsAll(map1).isOK());

        assertFalse(assertMap.not().containsAny(keys).isOK());
        assertTrue(assertMap.not().containsAny(map1).isOK());

        assertEquals("the map '{element1=1}' should NOT contain the key 'element1'", Assertor.that(map).not().contains(key1).getErrors());
        assertFalse(Assertor.that(map).not().contains(key1).isOK());
        assertFalse(Assertor.that(map).not().contains(key1, val1).isOK());
        assertFalse(Assertor.that(map).not().containsAll(map.keySet()).isOK());
        assertFalse(Assertor.that(map).not().containsAll(map).isOK());

        assertTrue(Assertor.that(map).not().contains(key1, (Integer) null).isOK());
        assertFalse(Assertor.that(map).not().containsAll(Arrays.asList("element3")).isOK());
        assertFalse(Assertor.that(map).not().containsAll(map1).isOK());

        assertTrue(Assertor.that(map).not().contains((String) null).isOK());
        assertTrue(Assertor.that(map).not().contains(key1, 3).isOK());
        assertTrue(Assertor.that(map).not().contains((String) null, (Integer) null).isOK());
        assertFalse(Assertor.that(map).not().containsAll((List<String>) null).isOK());
        assertFalse(Assertor.that(map).not().containsAll((Map<String, Integer>) null).isOK());

        assertFalse(Assertor.that((Map<String, Integer>) null).not().contains(key1).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).not().contains(key1, val1).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).not().containsAll(keys).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).not().containsAll(map1).isOK());
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

        assertTrue(Assertor.that(map).hasSize(1).isOK());
        assertFalse(Assertor.that(map).hasSize(0).isOK());
        assertFalse(Assertor.that(map).hasSize(2).isOK());

        assertFalse(Assertor.that(map).hasSize(-1).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).hasSize(1).isOK());
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

        assertFalse(Assertor.that(map).not().hasSize(1).isOK());
        assertTrue(Assertor.that(map).not().hasSize(0).isOK());
        assertTrue(Assertor.that(map).not().hasSize(2).isOK());

        assertFalse(Assertor.that(map).not().hasSize(-1).isOK());
        assertFalse(Assertor.that((Map<String, Integer>) null).not().hasSize(1).isOK());
    }
}
