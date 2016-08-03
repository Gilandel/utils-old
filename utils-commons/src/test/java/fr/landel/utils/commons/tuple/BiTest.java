/*-
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
package fr.landel.utils.commons.tuple;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check {@link Bi}
 *
 * @since 2 août 2016
 * @author Gilles
 *
 */
public class BiTest {

    /**
     * Test method for {@link Bi#getLeft()}, {@link Bi#getRight()},
     * {@link Bi#getKey()}, {@link Bi#getValue()}.
     */
    @Test
    public void testGet() {
        String key = "key";
        String value = "value";

        Bi<String> bi = Bi.of(key, value);

        assertNotNull(bi);
        assertEquals(key, bi.getKey());
        assertEquals(key, bi.getLeft());
        assertEquals(value, bi.getValue());
        assertEquals(value, bi.getRight());
    }

    /**
     * Test method for {@link Bi#equals(Object)}.
     */
    @Test
    public void testEquals() {
        String key = "key";
        String value = "value";

        Bi<String> bi = Bi.of(key, value);

        assertTrue(bi.equals(bi));
        assertTrue(bi.equals(Bi.of(key, value)));
        assertFalse(bi.equals(Bi.of(key, null)));
        assertFalse(bi.equals(Bi.of(null, value)));
        assertFalse(bi.equals(Bi.of(null, null)));
        Bi<String> biNull = null;
        assertFalse(bi.equals(biNull));
    }

    /**
     * Test method for {@link Bi#hashCode()}.
     */
    @Test
    public void testHashCode() {
        String key = "key";
        String value = "value";

        Bi<String> bi = Bi.of(key, value);

        assertEquals(key.hashCode() ^ value.hashCode(), bi.hashCode());
    }

    /**
     * Test method for {@link Bi#compareTo(Bi)}.
     */
    @Test
    public void testCompareTo() {
        String key = "key";
        String value = "value";

        Bi<String> bi = Bi.of(key, value);

        assertEquals(0, bi.compareTo(bi));
        assertEquals(0, bi.compareTo(Bi.of(key, value)));
        assertEquals(1, bi.compareTo(Bi.of(key, null)));
        assertEquals(1, bi.compareTo(Bi.of(null, value)));
        assertEquals(1, bi.compareTo(Bi.of(null, null)));
        assertEquals(Integer.MAX_VALUE, bi.compareTo(null));
    }

    /**
     * Test method for {@link Bi#toString()}.
     */
    @Test
    public void testToString() {
        String key = "key";
        String value = "value";

        Bi<String> bi = Bi.of(key, value);

        assertEquals("(key, value)", bi.toString());
    }

    /**
     * Test method for {@link Bi#toString(java.lang.String)}.
     */
    @Test
    public void testToStringString() {
        String key = "key";
        String value = "value";

        Bi<String> bi = Bi.of(key, value);

        assertEquals("key=value", bi.toString("%s=%s"));
    }

    /**
     * Test method for {@link Bi#ofMutable(java.lang.Object, java.lang.Object)}.
     */
    @Test
    public void testOfMutable() {
        String key = "key";
        String value = "value";
        String value2 = "value2";

        MutableBi<String> bi = Bi.ofMutable(key, value);

        assertEquals(value, bi.getValue());
        bi.setValue(value2);
        assertEquals(value2, bi.getValue());

        bi.setRight(value);
        assertEquals(value, bi.getValue());
        bi.setRight(value2);
        assertEquals(value2, bi.getValue());

        assertEquals(key, bi.getKey());
        bi.setLeft(value2);
        assertEquals(value2, bi.getKey());

        Bi<String> bii = Bi.of(key, value);
        try {
            bii.setValue("test");
            fail();
        } catch (UnsupportedOperationException e) {
            assertNotNull(e);
        }

    }
}
