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
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Test;

import fr.landel.utils.assertor.AssertIterable;
import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.assertor.Expect;

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
     * Test method for {@link AssertIterable#hasSize(int)}.
     * 
     * @throws IOException
     *             On not empty iterable
     */
    @Test
    public void testHasSize() throws IOException {
        final String el = "element";

        final Set<String> set = new HashSet<>();
        set.add(el);

        assertTrue(Assertor.that(set).hasSize(1).getResult());
        assertFalse(Assertor.that(set).hasSize(2).getResult());
        assertFalse(Assertor.that((Set<String>) null).hasSize(1).getResult());
        assertFalse(Assertor.that(set).hasSize(-1).getResult());

        final Iterable<String> iterable = new Iterable<String>() {
            @Override
            public Iterator<String> iterator() {
                return set.iterator();
            }
        };

        assertTrue(Assertor.that(iterable).hasSize(1).getResult());
        assertFalse(Assertor.that(iterable).hasSize(2).getResult());
    }

    /**
     * Test method for {@link AssertIterable#hasNotSize(int)}.
     * 
     * @throws IOException
     *             On not empty iterable
     */
    @Test
    public void testHasNotSize() throws IOException {
        final String el = "element";

        final Set<String> set = new HashSet<>();
        set.add(el);

        assertFalse(Assertor.that(set).hasNotSize(1).getResult());
        assertTrue(Assertor.that(set).hasNotSize(2).getResult());
        assertFalse(Assertor.that((Set<String>) null).hasNotSize(1).getResult());
        assertFalse(Assertor.that(set).hasNotSize(-1).getResult());

        final Iterable<String> iterable = new Iterable<String>() {
            @Override
            public Iterator<String> iterator() {
                return set.iterator();
            }
        };

        assertFalse(Assertor.that(iterable).hasNotSize(1).getResult());
        assertTrue(Assertor.that(iterable).hasNotSize(2).getResult());
    }

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

        Assertor.that(set).isEmpty().toThrow();

        set.add(el);

        Expect.exception(() -> {
            Assertor.that(set).isEmpty().toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(set).isEmpty().toThrow("iterable is not empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable is not empty");

        Expect.exception(() -> {
            Assertor.that(set).isEmpty().toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);
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

        Assertor.that(set).isNotEmpty().toThrow();

        set.clear();

        Expect.exception(() -> {
            Assertor.that(set).isNotEmpty().toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(set).isNotEmpty().toThrow("iterable is empty");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable is empty");

        Expect.exception(() -> {
            Assertor.that(set).isNotEmpty().toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that((Iterable<String>) null).isNotEmpty().toThrow();
            fail();
        }, IllegalArgumentException.class, "[Assertion failed] this iterable must be not empty.");
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

        Assertor.that(set).contains(el1).toThrow("iterable doesn't contain the element %p");

        Expect.exception(() -> {
            Assertor.that(set).contains(el2).toThrow("iterable doesn't contain the element %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable doesn't contain the element " + el2);

        Expect.exception(() -> {
            Assertor.that(set).contains(el2).toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that(set).contains((String) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the object to find must be not null.");

        set.clear();

        Expect.exception(() -> {
            Assertor.that(set).contains(el1).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(set).contains(el1).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must contain the object '" + el1 + "'.");

        Expect.exception(() -> {
            Assertor.that((Iterable<String>) null).contains(el1).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must be not null.");

        Expect.exception(() -> {
            Assertor.that(set).contains((String) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the object to find must be not null.");
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

        Assertor.that(set).contains(set2).toThrow("iterable doesn't contain the list %p");

        set2.add(el2);

        Expect.exception(() -> {
            Assertor.that(set).contains(set2).toThrow("iterable doesn't contain the list %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable doesn't contain the list " + set2.toString());

        Expect.exception(() -> {
            Assertor.that(set).contains(set2).toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that(set).contains((Iterable<String>) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the objects to find must be not null and not empty.");

        set.clear();

        Expect.exception(() -> {
            Assertor.that(set).contains(set2).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Assertor.that(set).contains(set2).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must contain all objects.");

        Expect.exception(() -> {
            Assertor.that((Iterable<String>) null).contains(el1).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must be not null.");

        Expect.exception(() -> {
            Assertor.that(set).contains((Iterable<String>) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the objects to find must be not null and not empty.");
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

        Assertor.that(set).doesNotContain(el2).toThrow("iterable contains the element %p");

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain(el1).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must not contain the object '" + el1 + "'.");

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain(el1).toThrow("iterable contains the element %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable contains the element " + el1);

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain(el1).toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain((String) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the object to find must be not null.");

        set.clear();

        Expect.exception(() -> {
            Assertor.that((Iterable<String>) null).doesNotContain(el1).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must be not null.");
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

        Assertor.that(set).doesNotContain(set2).toThrow("iterable contains the list %p");

        set.add(el2);

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain(set2).toThrow("iterable contains the list %2$p");
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] iterable contains the list " + set2.toString());

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain(set2).toThrow(new IOException());
            fail(ERROR);
        }, IOException.class);

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain((Iterable<String>) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the objects to find must be not null and not empty.");

        set.clear();

        Assertor.that(set).doesNotContain(set2).toThrow();

        Expect.exception(() -> {
            Assertor.that((Iterable<String>) null).doesNotContain(set2).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the iterable must be not null.");

        Expect.exception(() -> {
            Assertor.that(set).doesNotContain((Iterable<String>) null).toThrow();
            fail(ERROR);
        }, IllegalArgumentException.class, "[Assertion failed] the objects to find must be not null and not empty.");
    }

    /**
     * Test method for
     * {@link AssertIterable#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQString() {
        try {
            Assertor.that(Arrays.asList("")).isNotEmpty().toThrow("empty collection");
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for
     * {@link AssertIterable#isNotEmpty(java.util.Collection, String, Object...)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQString() {
        Assertor.that(Collections.emptyList()).isNotEmpty().toThrow("empty collection");
    }

    /**
     * Test method for {@link AssertIterable#isNotEmpty(java.util.Collection)} .
     */
    @Test
    public void testIsNotEmptyOKCollectionOfQ() {
        try {
            Assertor.that(Arrays.asList("")).isNotEmpty().toThrow();
        } catch (IllegalArgumentException e) {
            fail("The test isn't correct");
        }
    }

    /**
     * Test method for {@link AssertIterable#isNotEmpty(java.util.Collection)} .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsNotEmptyKOCollectionOfQ() {
        Assertor.that(Collections.emptyList()).isNotEmpty().toThrow();
    }
}
