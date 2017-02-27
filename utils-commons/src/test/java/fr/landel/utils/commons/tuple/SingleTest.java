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
package fr.landel.utils.commons.tuple;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check {@link Single}
 *
 * @since Aug 2, 2016
 * @author Gilles
 *
 */
public class SingleTest {

    /**
     * Test method for {@link Single#get()}.
     */
    @Test
    public void testGet() {
        String element = "element";

        Single<String> mono = Single.of(element);

        assertNotNull(mono);
        assertEquals(element, mono.get());
    }

    /**
     * Test method for {@link Single#equals(Object)}.
     */
    @Test
    public void testEquals() {
        String element = "element";

        Single<String> mono = Single.of(element);

        assertTrue(mono.equals(mono));
        assertTrue(mono.equals(Single.of(element)));
        assertFalse(mono.equals(Single.of(null)));

        Single<String> monoNull = null;
        assertFalse(mono.equals(monoNull));
    }

    /**
     * Test method for {@link Single#hashCode()}.
     */
    @Test
    public void testHashCode() {
        String element = "element";

        Single<String> mono = Single.of(element);

        assertEquals(element.hashCode(), mono.hashCode());
    }

    /**
     * Test method for {@link Single#compareTo(Single)}.
     */
    @Test
    public void testCompareTo() {
        String element = "element";

        Single<String> mono = Single.of(element);

        assertEquals(0, mono.compareTo(mono));
        assertEquals(0, mono.compareTo(Single.of(element)));
        assertEquals(1, mono.compareTo(Single.of(null)));
        assertEquals(Integer.MAX_VALUE, mono.compareTo(null));
    }

    /**
     * Test method for {@link Single#toString()}.
     */
    @Test
    public void testToString() {
        String element = "element";

        Single<String> mono = Single.of(element);

        assertEquals("(element)", mono.toString());
    }

    /**
     * Test method for {@link Single#toString(java.lang.String)}.
     */
    @Test
    public void testToStringString() {
        String element = "element";

        Single<String> mono = Single.of(element);

        assertEquals("element", mono.toString("%s"));
    }

    /**
     * Test method for {@link Single#ofMutable(java.lang.Object)}.
     */
    @Test
    public void testOfMutable() {
        String element1 = "value";
        String element2 = "value2";

        MutableSingle<String> mono = Single.ofMutable(element1);

        assertEquals(element1, mono.get());
        mono.set(element2);
        assertEquals(element2, mono.get());
        mono.set(null);
        assertNull(mono.get());

        Single<String> monoImm = Single.of(element1);
        try {
            monoImm.set("test");
            fail();
        } catch (UnsupportedOperationException e) {
            assertNotNull(e);
        }
    }
}
