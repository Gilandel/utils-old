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
package fr.landel.utils.commons.over;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

/**
 * Check methods from over comparable and over object classes
 *
 * @since Apr 23, 2016
 * @author Gilles
 *
 */
public class AbstractOverComparableTest {

    /**
     * The comparable implementation
     *
     * @since Apr 23, 2016
     * @author Gilles
     *
     */
    private class ComparableImpl extends AbstractOverComparable<ComparableImpl> {

        private Integer value;
        private String value2;

        /**
         * Constructor
         */
        public ComparableImpl() {
            super();
        }

        /**
         * Constructor
         *
         * @param value
         *            The comparable value
         */
        public ComparableImpl(final Integer value) {
            super(ComparableImpl.class);
            this.value = value;
        }

        /**
         * Constructor
         *
         * @param value
         *            The unused value
         */
        public ComparableImpl(final String value) {
            super(null);
            this.value2 = value;
        }

        /**
         * @return value 2
         */
        public String getValue2() {
            return this.value2;
        }

        @Override
        protected int overCompareTo(final ComparableImpl obj) {
            return this.value.compareTo(obj.value);
        }

        @Override
        protected void overToString(final StringBuilder sb) {
            final Map<String, Object> map = new HashMap<>();

            this.overToStringMap(sb, map);

            ComparableImpl.mapToString(sb, map);
        }

        protected void overToStringMap(final StringBuilder sb, final Map<String, Object> map) {
            map.put("value", this.value);
        }

        @Override
        protected boolean overEquals(final ComparableImpl obj) {
            return this.value.equals(obj.value);
        }

        @Override
        protected int overHashCode() {
            return this.value;
        }

    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.over.AbstractOverComparable#overCompareTo(fr.landel.utils.commons.over.AbstractOverComparable)}
     * .
     */
    @Test
    public void testOverCompareTo() {
        final int key1 = 10;
        final int key2 = 9;

        final ComparableImpl comparable1 = new ComparableImpl(key1);
        final ComparableImpl comparable2 = new ComparableImpl(key2);

        assertEquals(-1, comparable1.compareTo(null));
        assertEquals(0, comparable1.compareTo(comparable1));
        assertEquals(1, comparable1.compareTo(comparable2));

        // simple check
        assertNotNull(new ComparableImpl());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.over.AbstractOverObject#getOverClass()}.
     */
    @Test
    public void testGetOverClass() {
        final ComparableImpl comparable = new ComparableImpl(1);

        assertEquals(ComparableImpl.class, comparable.getOverClass());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.over.AbstractOverObject#overToString(java.lang.StringBuilder)}
     * .
     */
    @Test
    public void testOverToString() {
        final int key1 = 10;

        ComparableImpl comparable = new ComparableImpl(key1);

        assertEquals("ComparableImpl [value=10]", comparable.toString());

        comparable = new ComparableImpl("");

        assertEquals("", comparable.getValue2());
        assertEquals("ComparableImpl [value=null]", comparable.toString());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.over.AbstractOverObject#overEquals(fr.landel.utils.commons.over.AbstractOverObject)}
     * .
     */
    @Test
    public void testOverEquals() {
        final int key1 = 10;
        final int key2 = 9;

        final ComparableImpl comparable1 = new ComparableImpl(key1);
        final ComparableImpl comparable2 = new ComparableImpl(key2);

        ComparableImpl testNull = new ComparableImpl(1);
        if (testNull.value > 0) {
            testNull = null;
        }

        assertFalse(comparable1.equals(testNull));
        assertFalse(comparable1.equals(comparable2));
        assertTrue(comparable1.equals(comparable1));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.commons.over.AbstractOverObject#overHashCode()}.
     */
    @Test
    public void testOverHashCode() {
        final int key1 = 10;

        final ComparableImpl comparable1 = new ComparableImpl(key1);

        assertEquals(key1, comparable1.hashCode());
    }
}
