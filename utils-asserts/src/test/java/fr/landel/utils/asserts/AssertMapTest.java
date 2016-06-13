/*-
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.asserts;

import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

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

        final AssertMap<String, Integer> assertMap = AssertUtils.check(map);

        assertMap.isEmpty();
        assertMap.isEmpty("map is not empty");
        assertMap.isEmpty(new IOException());

        map.put(el, 1);

        Expect.exception(() -> {
            assertMap.isEmpty();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            assertMap.isEmpty("map is not empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] map is not empty");

        Expect.exception(() -> {
            assertMap.isEmpty(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            AssertUtils.check((Map<String, Integer>) null).isEmpty("this argument is required; it must not be null");
            fail();
        }, IllegalArgumentException.class, "[Assertion failed] this argument is required; it must not be null");
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

        final AssertMap<String, Integer> assertMap = AssertUtils.check(map);

        assertMap.isNotEmpty();
        assertMap.isNotEmpty("map is empty");
        assertMap.isNotEmpty(new IOException());

        map.clear();

        Expect.exception(() -> {
            assertMap.isNotEmpty();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            assertMap.isNotEmpty("map is empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] map is empty");

        Expect.exception(() -> {
            assertMap.isNotEmpty(new IOException());
            fail(ERROR);
        }, IOException.class);
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
        final Integer val1 = 1;

        final Map<String, Integer> map = new HashMap<>();
        map.put(key1, val1);

        final AssertMap<String, Integer> assertMap = AssertUtils.check(map);

        assertMap.isNotNull().contains(key1);

        assertMap.contains(key1);
        assertMap.contains(key1, "map doesn't contain the element %2$p");
        assertMap.contains(key1, new IOException());
        assertMap.contains(key1, val1);
        assertMap.contains(key1, val1, "map doesn't contain the element %3$p");
        assertMap.contains(key1, val1, new IOException());
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

        final AssertMap<String, Integer> assertMap = AssertUtils.check(map);

        assertMap.isNotNull().contains(key1);

        assertMap.doesNotContain(key2);
        assertMap.doesNotContain(key2, "map contains the element %2$p");
        assertMap.doesNotContain(key2, new IOException());
        assertMap.doesNotContain(key2, val2);
        assertMap.doesNotContain(key2, val2, "map contains the element %3$p");
        assertMap.doesNotContain(key2, val2, new IOException());
    }
}