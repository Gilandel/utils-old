/*-
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

import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;

/**
 * Check {@link Default}
 *
 * @since Jan 28, 2017
 * @author Gilles
 *
 */
public class DefaultTest {

    private boolean consumerCheck = false;

    /**
     * Test method for {@link Default#hashCode()}.
     */
    @Test
    public void testHashCode() {
        final int hc = 31 * 31;
        assertEquals(hc, Default.empty(0).hashCode());
        assertEquals(hc, Default.ofNullable(null, 0).hashCode());
        assertEquals(Objects.hash("test", "test"), Default.of("test").hashCode());
    }

    /**
     * Test method for {@link Default#empty()}.
     */
    @Test
    public void testEmpty() {
        assertFalse(Default.empty("default").isPresent());
        assertEquals("default", Default.empty("default").get());

        try {
            Default.empty(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#of(java.lang.Object)}.
     */
    @Test
    public void testOf() {
        assertEquals(5, (int) Default.of(5).get());
        assertTrue(Default.of(5).isPresent());
        try {
            Default.of(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#ofNullable(java.lang.Object)}.
     */
    @Test
    public void testOfNullable() {
        assertEquals(5, (int) Default.ofNullable(5, 2).get());
        assertTrue(Default.ofNullable(5, 2).isPresent());
        assertEquals(2, (int) Default.ofNullable(null, 2).get());
        assertFalse(Default.ofNullable(null, 2).isPresent());
    }

    /**
     * Test method for {@link Default#get()}.
     */
    @Test
    public void testGet() {
        assertEquals(5, (int) Default.of(5).get());
        assertEquals(5, (int) Default.ofNullable(5, 2).get());
        assertEquals(2, (int) Default.ofNullable(null, 2).get());
        assertEquals(2, (int) Default.empty(2).get());
    }

    /**
     * Test method for {@link Default#getDefault()}.
     */
    @Test
    public void testGetDefault() {
        assertEquals(5, (int) Default.of(5).getDefault());
        assertEquals(2, (int) Default.ofNullable(5, 2).getDefault());
        assertEquals(2, (int) Default.ofNullable(null, 2).getDefault());
        assertEquals(2, (int) Default.empty(2).getDefault());
    }

    /**
     * Test method for {@link Default#isPresent()}.
     */
    @Test
    public void testIsPresent() {
        assertTrue(Default.of(5).isPresent());
        assertFalse(Default.ofNullable(null, 2).isPresent());
        assertFalse(Default.empty(2).isPresent());
    }

    /**
     * Test method for {@link Default#ifPresent(java.util.function.Consumer)}.
     */
    @Test
    public void testIfPresent() {
        final Consumer<Object> consumer = v -> this.consumerCheck = true;

        this.consumerCheck = false;
        Default.of(5).ifPresent(consumer);
        assertTrue(this.consumerCheck);

        this.consumerCheck = false;
        Default.ofNullable(null, 2).ifPresent(consumer);
        assertFalse(this.consumerCheck);

        this.consumerCheck = false;
        Default.empty(2).ifPresent(consumer);
        assertFalse(this.consumerCheck);

        try {
            Default.of(5).ifPresent(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#ifAbsent(Consumer)}.
     */
    @Test
    public void testIfAbsent() {
        final Consumer<Object> consumer = v -> this.consumerCheck = true;

        this.consumerCheck = false;
        Default.of(5).ifAbsent(consumer);
        assertFalse(this.consumerCheck);

        this.consumerCheck = false;
        Default.ofNullable(null, 2).ifAbsent(consumer);
        assertTrue(this.consumerCheck);

        this.consumerCheck = false;
        Default.empty(2).ifAbsent(consumer);
        assertTrue(this.consumerCheck);

        try {
            Default.of(5).ifAbsent(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#filter(java.util.function.Predicate)}.
     */
    @Test
    public void testFilter() {
        assertTrue(Default.of(5).filter(v -> v == 5).isPresent());
        assertEquals(5, (int) Default.of(5).filter(v -> v == 5).get());
        assertFalse(Default.of(5).filter(v -> v == 4).isPresent());

        assertTrue(Default.ofNullable(5, 2).filter(v -> v == 5).isPresent());
        assertEquals(5, (int) Default.ofNullable(5, 2).filter(v -> v == 5).get());
        assertFalse(Default.ofNullable(5, 2).filter(v -> v == 4).isPresent());

        assertFalse(Default.ofNullable(null, 2).filter(v -> v == null).isPresent());
        assertEquals(2, (int) Default.ofNullable(null, 2).filter(v -> v == null).get());
        assertFalse(Default.ofNullable(null, 2).filter(v -> v != null).isPresent());

        assertFalse(Default.empty(2).filter(v -> v == null).isPresent());
        assertFalse(Default.empty(2).filter(v -> v != null).isPresent());
        assertEquals(2, (int) Default.empty(2).filter(v -> v == null).get());

        try {
            Default.of(5).filter(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#map(java.util.function.Function)}.
     */
    @Test
    public void testMap() {
        final Function<Object, Integer> mapper = v -> {
            try {
                return Integer.parseInt(String.valueOf(v));
            } catch (NumberFormatException e) {
                return 0;
            }
        };

        assertEquals(5, (int) Default.of("5").map(mapper).get());
        assertEquals(0, (int) Default.of("").map(mapper).get());

        assertEquals(5, (int) Default.ofNullable("5", "2").map(mapper).get());
        assertEquals(0, (int) Default.ofNullable("", "2").map(mapper).get());
        assertEquals(2, (int) Default.ofNullable(null, "2").map(mapper).get());

        assertFalse(Default.empty("").map(mapper).isPresent());
        assertFalse(Default.empty(2).map(v -> true).isPresent());

        try {
            Default.of(5).map(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#flatMap(java.util.function.Function)}.
     */
    @Test
    public void testFlatMap() {
        final Function<Object, Default<Integer>> mapper = v -> {
            try {
                return Default.of(Integer.parseInt(String.valueOf(v)));
            } catch (NumberFormatException e) {
                return Default.of(0);
            }
        };

        assertEquals(5, (int) Default.of("5").flatMap(mapper).get());
        assertEquals(0, (int) Default.of("").flatMap(mapper).get());

        assertEquals(5, (int) Default.ofNullable("5", "2").flatMap(mapper).get());
        assertEquals(0, (int) Default.ofNullable("", "2").flatMap(mapper).get());
        assertEquals(2, (int) Default.ofNullable(null, "2").flatMap(mapper).get());

        assertTrue(Default.empty("2").flatMap(mapper).isPresent());
        assertTrue(Default.empty(2).flatMap(v -> Default.of(true)).isPresent());

        try {
            Default.of(5).map(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#orElse(java.lang.Object)}.
     */
    @Test
    public void testOrElse() {
        assertEquals(5, (int) Default.of(5).orElse(3));
        assertEquals(3, (int) Default.ofNullable(null, 2).orElse(3));
        assertEquals(3, (int) Default.empty(2).orElse(3));
        assertNull(Default.empty(2).orElse(null));
    }

    /**
     * Test method for {@link Default#orElseGet(java.util.function.Supplier)}.
     */
    @Test
    public void testOrElseGet() {
        Supplier<String> t = () -> "titi";
        Default.of((CharSequence) "test").orElseGet(t);

        assertEquals(5, (int) Default.of(5).orElseGet(() -> 3));
        assertEquals(3, (int) Default.ofNullable(null, 2).orElseGet(() -> 3));
        assertEquals(3, (int) Default.empty(2).orElseGet(() -> 3));

        try {
            Default.empty(2).orElseGet(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            Default.of(5).orElseGet(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#orElseThrow(java.util.function.Supplier)}.
     */
    @Test
    public void testOrElseThrow() {
        final String error = "result incorrect";
        final Supplier<IllegalArgumentException> exception = () -> new IllegalArgumentException(error);

        assertEquals(5, (int) Default.of(5).orElseThrow(exception));
        assertEquals(5, (int) Default.ofNullable(5, 2).orElseThrow(exception));

        try {
            Default.ofNullable(null, 2).orElseThrow(exception);
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals(error, e.getMessage());
        }

        try {
            Default.empty(2).orElseThrow(exception);
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals(error, e.getMessage());
        }

        try {
            Default.empty(2).orElseThrow(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            Default.of(2).orElseThrow(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Default#equals(java.lang.Object)}.
     */
    @Test
    public void testEqualsObject() {
        Default<Integer> d = Default.of(5);
        assertTrue(d.equals(d));
        assertTrue(Default.of(5).equals(Default.of(5)));
        assertFalse(Default.of(5).equals(Default.ofNullable(5, 2)));
        assertFalse(Default.ofNullable(5, 2).equals(Default.of(5)));
        assertFalse(Default.of(5).equals(Default.empty(2)));
        assertFalse(Default.ofNullable(5, 2).equals(Default.empty(2)));
        assertTrue(Default.ofNullable(5, 2).equals(Default.ofNullable(5, 2)));
        assertTrue(Default.ofNullable(null, 2).equals(Default.empty(2)));
        assertTrue(Default.empty(2).equals(Default.empty(2)));

        assertFalse(Default.ofNullable(5, 2).equals(5));
    }

    /**
     * Test method for {@link Default#toString()}.
     */
    @Test
    public void testToString() {
        assertEquals("Default: 5 otherwise 5", Default.of(5).toString());
        assertEquals("Default: 5 otherwise 2", Default.ofNullable(5, 2).toString());
        assertEquals("Default: null otherwise 2", Default.ofNullable(null, 2).toString());
        assertEquals("Default: null otherwise 2", Default.empty(2).toString());
    }
}
