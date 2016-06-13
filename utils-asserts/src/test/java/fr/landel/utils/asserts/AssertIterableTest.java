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
import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

/**
 * Check {@link AssertIterable}
 *
 * @since 4 juin 2016
 * @author Gilles
 *
 */
public class AssertIterableTest {

    private final String ERROR = "error expected";

    /**
     * Test method for {@link AssertIterable#isEmpty}.
     * 
     * @throws IOException
     *             On not empty iterable
     */
    @Test
    public void testIsEmpty() throws IOException {
        final String el = "element";

        final Set<String> set = new HashSet<>();

        AssertUtils.check(set).isEmpty().isEmpty("iterable is not empty").isEmpty(new IOException());

        set.add(el);

        Expect.exception(() -> {
            AssertUtils.check(set).isEmpty();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            AssertUtils.check(set).isEmpty("iterable is not empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable is not empty");

        Expect.exception(() -> {
            AssertUtils.check(set).isEmpty(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            AssertUtils.check((Iterable<String>) null).isEmpty();
            fail();
        }, IllegalArgumentException.class, "[Assertion failed] this iterable must be empty and not null");
    }

    /**
     * Test method for {@link AssertIterable#isNotEmpty}.
     * 
     * @throws IOException
     *             On empty iterable
     */
    @Test
    public void testIsNotEmpty() throws IOException {
        final String el = "element";

        final Set<String> set = new HashSet<>();
        set.add(el);

        AssertUtils.check(set).isNotEmpty().isNotEmpty("iterable is empty").isNotEmpty(new IOException());

        set.clear();

        Expect.exception(() -> {
            AssertUtils.check(set).isNotEmpty();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            AssertUtils.check(set).isNotEmpty("iterable is empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable is empty");

        Expect.exception(() -> {
            AssertUtils.check(set).isNotEmpty(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            AssertUtils.check((Iterable<String>) null).isNotEmpty();
            fail();
        }, IllegalArgumentException.class, "[Assertion failed] this iterable must not be empty: it must contain at least 1 element");
    }

    /**
     * Test method for {@link AssertIterable#contains}.
     * 
     * @throws IOException
     *             On not contain
     */
    @Test
    public void testContains() throws IOException {
        final String el1 = "element1";
        final String el2 = "element2";

        final Set<String> set = new HashSet<>();
        set.add(el1);

        AssertUtils.check(set).contains(el1).contains(el1, "iterable doesn't contain the element %p").contains(el1, new IOException());

        Expect.exception(() -> {
            AssertUtils.check(set).contains(el2, "iterable doesn't contain the element %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable doesn't contain the element " + el2);

        Expect.exception(() -> {
            AssertUtils.check(set).contains(el2, new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            AssertUtils.check(set).contains((String) null);
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] this argument is required; it must not be null");

        set.clear();

        Expect.exception(() -> {
            AssertUtils.check(set).contains(el1);
            fail(ERROR);
        }, IllegalArgumentException.class);

        final String expectedMessage = "[Assertion failed] this iterable must not be empty: it must contain at least 1 element";

        Expect.exception(() -> {
            AssertUtils.check(set).contains(el1);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check((Iterable<String>) null).contains(el1);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check(set).contains((String) null);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);
    }

    /**
     * Test method for {@link AssertIterable#contains}.
     * 
     * @throws IOException
     *             On not contain
     */
    @Test
    public void testContainsIterable() throws IOException {
        final String el1 = "element1";
        final String el2 = "element2";

        final Set<String> set = new HashSet<>();
        final Set<String> set2 = new HashSet<>();
        set.add(el1);
        set2.add(el1);

        AssertUtils.check(set).contains(set2).contains(set2, "iterable doesn't contain the list %p").contains(set2, new IOException());

        set2.add(el2);

        Expect.exception(() -> {
            AssertUtils.check(set).contains(set2, "iterable doesn't contain the list %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable doesn't contain the list " + set2.toString());

        Expect.exception(() -> {
            AssertUtils.check(set).contains(set2, new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            AssertUtils.check(set).contains((Iterable<String>) null);
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] this iterable must not be empty: it must contain at least 1 element");

        set.clear();

        Expect.exception(() -> {
            AssertUtils.check(set).contains(set2);
            fail(ERROR);
        }, IllegalArgumentException.class);

        final String expectedMessage = "[Assertion failed] this iterable must not be empty: it must contain at least 1 element";

        Expect.exception(() -> {
            AssertUtils.check(set).contains(set2);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check((Iterable<String>) null).contains(el1);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check(set).contains((Iterable<String>) null);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);
    }

    /**
     * Test method for {@link AssertIterable#doesNotContain}.
     * 
     * @throws IOException
     *             On contain
     */
    @Test
    public void testDoesNotContain() throws IOException {
        final String el1 = "element1";
        final String el2 = "element2";

        final Set<String> set = new HashSet<>();
        set.add(el1);

        AssertUtils.check(set).doesNotContain(el2).doesNotContain(el2, "iterable contains the element %p").doesNotContain(el2,
                new IOException());

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(el1, "iterable contains the element %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable contains the element " + el1);

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(el1, new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain((String) null);
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] this argument is required; it must not be null");

        set.clear();

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(el1);
            fail(ERROR);
        }, IllegalArgumentException.class);

        final String expectedMessage = "[Assertion failed] this iterable must not be empty: it must contain at least 1 element";

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(el1);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check((Iterable<String>) null).doesNotContain(el1);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain((String) null);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);
    }

    /**
     * Test method for {@link AssertIterable#doesNotContain}.
     * 
     * @throws IOException
     *             On contain
     */
    @Test
    public void testDoesNotContainIterable() throws IOException {
        final String el1 = "element1";
        final String el2 = "element2";

        final Set<String> set = new HashSet<>();
        final Set<String> set2 = new HashSet<>();
        set.add(el1);
        set2.add(el2);

        AssertUtils.check(set).doesNotContain(set2).doesNotContain(set2, "iterable contains the list %p").doesNotContain(set2,
                new IOException());

        set.add(el2);

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(set2, "iterable contains the list %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable contains the list " + set2.toString());

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(set2, new IOException());
            fail(ERROR);
        }, IOException.class);

        final String expectedMessage = "[Assertion failed] this iterable must not be empty: it must contain at least 1 element";

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain((Iterable<String>) null);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        set.clear();

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(set2);
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain(set2);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check((Iterable<String>) null).doesNotContain(set2);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);

        Expect.exception(() -> {
            AssertUtils.check(set).doesNotContain((Iterable<String>) null);
            fail(ERROR);
        }, IllegalArgumentException.class, expectedMessage);
    }
}