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

import java.util.NoSuchElementException;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Test;

/**
 * Check {@link Result}
 *
 * @since Jan 28, 2017
 * @author Gilles
 *
 */
public class ResultTest {

    private boolean consumerCheck = false;

    /**
     * Test method for {@link Result#hashCode()}.
     */
    @Test
    public void testHashCode() {
        assertEquals(0, Result.empty().hashCode());
        assertEquals(0, Result.ofNullable(null).hashCode());
        assertEquals("test".hashCode(), Result.of("test").hashCode());
    }

    /**
     * Test method for {@link Result#empty()}.
     */
    @Test
    public void testEmpty() {
        assertFalse(Result.empty().isPresent());
        try {
            Result.empty().get();
        } catch (NoSuchElementException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#of(java.lang.Object)}.
     */
    @Test
    public void testOf() {
        assertEquals(5, (int) Result.of(5).get());
        assertTrue(Result.of(5).isPresent());
        try {
            Result.of(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#ofNullable(java.lang.Object)}.
     */
    @Test
    public void testOfNullable() {
        assertEquals(5, (int) Result.ofNullable(5).get());
        assertTrue(Result.ofNullable(5).isPresent());
        assertNull(Result.ofNullable(null).get());
        assertTrue(Result.ofNullable(null).isPresent());
    }

    /**
     * Test method for {@link Result#get()}.
     */
    @Test
    public void testGet() {
        assertEquals(5, (int) Result.of(5).get());

        try {
            Result.empty().get();
        } catch (NoSuchElementException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#isNull()}.
     */
    @Test
    public void testIsNull() {
        assertFalse(Result.of(5).isNull());
        assertTrue(Result.ofNullable(null).isNull());
        assertTrue(Result.empty().isNull());
    }

    /**
     * Test method for {@link Result#isNotNull()}.
     */
    @Test
    public void testIsNotNull() {
        assertTrue(Result.of(5).isNotNull());
        assertFalse(Result.ofNullable(null).isNotNull());
        assertFalse(Result.empty().isNotNull());
    }

    /**
     * Test method for {@link Result#ifNotNull(Consumer)}.
     */
    @Test
    public void testIfNotNull() {
        final Consumer<Object> consumer = v -> this.consumerCheck = true;

        this.consumerCheck = false;
        Result.of(5).ifNotNull(consumer);
        assertTrue(this.consumerCheck);

        this.consumerCheck = false;
        Result.ofNullable(null).ifNotNull(consumer);
        assertFalse(this.consumerCheck);

        this.consumerCheck = false;
        Result.empty().ifNotNull(consumer);
        assertFalse(this.consumerCheck);

        try {
            Result.of(5).ifNotNull(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#isPresent()}.
     */
    @Test
    public void testIsPresent() {
        assertTrue(Result.of(5).isPresent());
        assertTrue(Result.ofNullable(null).isPresent());
        assertFalse(Result.empty().isPresent());
    }

    /**
     * Test method for {@link Result#ifPresent(java.util.function.Consumer)}.
     */
    @Test
    public void testIfPresent() {
        final Consumer<Object> consumer = v -> this.consumerCheck = true;

        this.consumerCheck = false;
        Result.of(5).ifPresent(consumer);
        assertTrue(this.consumerCheck);

        this.consumerCheck = false;
        Result.ofNullable(null).ifPresent(consumer);
        assertTrue(this.consumerCheck);

        this.consumerCheck = false;
        Result.empty().ifPresent(consumer);
        assertFalse(this.consumerCheck);

        try {
            Result.of(5).ifPresent(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#filter(java.util.function.Predicate)}.
     */
    @Test
    public void testFilter() {
        assertTrue(Result.of(5).filter(v -> v == 5).isPresent());
        assertEquals(5, (int) Result.of(5).filter(v -> v == 5).get());
        assertFalse(Result.of(5).filter(v -> v == 4).isPresent());

        assertTrue(Result.ofNullable(5).filter(v -> v == 5).isPresent());
        assertEquals(5, (int) Result.ofNullable(5).filter(v -> v == 5).get());
        assertFalse(Result.ofNullable(5).filter(v -> v == 4).isPresent());

        assertTrue(Result.ofNullable(null).filter(v -> v == null).isPresent());
        assertNull(Result.ofNullable(null).filter(v -> v == null).get());
        assertFalse(Result.ofNullable(null).filter(v -> v != null).isPresent());

        assertFalse(Result.empty().filter(v -> v == null).isPresent());
        assertFalse(Result.empty().filter(v -> v != null).isPresent());
        try {
            Result.empty().filter(v -> v == null).get();
            fail();
        } catch (NoSuchElementException e) {
            assertNotNull(e);
        }

        try {
            Result.of(5).filter(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#map(java.util.function.Function)}.
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

        assertEquals(5, (int) Result.of("5").map(mapper).get());
        assertEquals(0, (int) Result.of("").map(mapper).get());

        assertEquals(5, (int) Result.ofNullable("5").map(mapper).get());
        assertEquals(0, (int) Result.ofNullable("").map(mapper).get());
        assertEquals(0, (int) Result.ofNullable(null).map(mapper).get());

        assertFalse(Result.empty().map(mapper).isPresent());
        assertFalse(Result.empty().map(v -> true).isPresent());

        try {
            Result.of(5).map(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#flatMap(java.util.function.Function)}.
     */
    @Test
    public void testFlatMap() {
        final Function<Object, Result<Integer>> mapper = v -> {
            try {
                return Result.of(Integer.parseInt(String.valueOf(v)));
            } catch (NumberFormatException e) {
                return Result.of(0);
            }
        };

        assertEquals(5, (int) Result.of("5").flatMap(mapper).get());
        assertEquals(0, (int) Result.of("").flatMap(mapper).get());

        assertEquals(5, (int) Result.ofNullable("5").flatMap(mapper).get());
        assertEquals(0, (int) Result.ofNullable("").flatMap(mapper).get());
        assertEquals(0, (int) Result.ofNullable(null).flatMap(mapper).get());

        assertFalse(Result.empty().flatMap(mapper).isPresent());
        assertFalse(Result.empty().flatMap(v -> Result.of(true)).isPresent());

        try {
            Result.of(5).map(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#orElse(java.lang.Object)}.
     */
    @Test
    public void testOrElse() {
        assertEquals(5, (int) Result.of(5).orElse(3));
        assertNull(Result.ofNullable(null).orElse(3));
        assertEquals(3, (int) Result.empty().orElse(3));
        assertNull(Result.empty().orElse(null));
    }

    /**
     * Test method for {@link Result#orElseGet(java.util.function.Supplier)}.
     */
    @Test
    public void testOrElseGet() {
        assertEquals(5, (int) Result.of(5).orElseGet(() -> 3));
        assertNull(Result.ofNullable(null).orElseGet(() -> 3));
        assertEquals(3, (int) Result.empty().orElseGet(() -> 3));

        try {
            Result.empty().orElseGet(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            Result.of(5).orElseGet(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#orElseThrow(java.util.function.Supplier)}.
     */
    @Test
    public void testOrElseThrow() {
        final String error = "result incorrect";
        final Supplier<IllegalArgumentException> exception = () -> new IllegalArgumentException(error);

        assertEquals(5, (int) Result.of(5).orElseThrow(exception));
        assertNull(Result.ofNullable(null).orElseThrow(exception));

        try {
            Result.empty().orElseThrow(exception);
            fail();
        } catch (IllegalArgumentException e) {
            assertEquals(error, e.getMessage());
        }

        try {
            Result.empty().orElseThrow(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            Result.of(5).orElseThrow(null);
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }

    /**
     * Test method for {@link Result#equals(java.lang.Object)}.
     */
    @Test
    public void testEqualsObject() {
        assertTrue(Result.of(5).equals(Result.of(5)));
        assertTrue(Result.of(5).equals(Result.ofNullable(5)));
        assertTrue(Result.ofNullable(5).equals(Result.of(5)));
        assertFalse(Result.of(5).equals(Result.empty()));
        assertFalse(Result.ofNullable(5).equals(Result.empty()));
        assertFalse(Result.ofNullable(null).equals(Result.empty()));
        assertTrue(Result.empty().equals(Result.empty()));

        assertFalse(Result.ofNullable(5).equals(5));
    }

    /**
     * Test method for {@link Result#toString()}.
     */
    @Test
    public void testToString() {
        assertEquals("Result: 5", Result.of(5).toString());
        assertEquals("Result: 5", Result.ofNullable(5).toString());
        assertEquals("Result: null", Result.ofNullable(null).toString());
        assertEquals("Result.empty", Result.empty().toString());
    }
}
