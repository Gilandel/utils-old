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
package fr.landel.utils.commons.builder;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Check {@link EqualsBuilder2}
 *
 * @since Feb 11, 2017
 * @author Gilles
 *
 */
public class EqualsBuilder2Test {

    /**
     * Test method for {@link EqualsBuilder2#build()}.
     */
    @Test
    public void testBuild() {
        IllegalArgumentException e1 = new IllegalArgumentException("error");
        NullPointerException e2 = new NullPointerException("error");
        IllegalArgumentException e3 = new IllegalArgumentException("ERROR");

        assertTrue(new EqualsBuilder2<>(e1, e1).build());

        assertTrue(new EqualsBuilder2<>(e1, e1).isEqual());
        assertFalse(new EqualsBuilder2<>(e1, e2).isEqual()); // class diff
        assertTrue(new EqualsBuilder2<>(e1, e2, Exception.class).isEqual());
        assertTrue(new EqualsBuilder2<>(e1, e3).isEqual());
        assertFalse(new EqualsBuilder2<>(e1, null, Exception.class).isEqual());

        try {
            new EqualsBuilder2<>(null, null).isEqual();
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        try {
            new EqualsBuilder2<>(null, e2).isEqual();
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        assertFalse(new EqualsBuilder2<>(e1, null).isEqual());

        assertFalse(new EqualsBuilder2<>(e1, e2).append(e -> e.getMessage()).isEqual());
        assertTrue(new EqualsBuilder2<>(e1, e2, Exception.class).append(e -> e.getMessage()).isEqual());
        assertFalse(new EqualsBuilder2<>(e1, e3).append(e -> e.getMessage()).isEqual());
        assertTrue(new EqualsBuilder2<>(e1, e3).append(e -> e.getMessage(), (a, b) -> a.equalsIgnoreCase(b)).isEqual());

        assertFalse(new EqualsBuilder2<>(e1, e2).append("toto", "titi").isEqual());
        assertTrue(new EqualsBuilder2<>(e1, e3).append("toto", "titi", e -> e.length()).isEqual());
        assertTrue(new EqualsBuilder2<>(e1, e3).append("toto", "titi", e -> e.length()).append("toto", "toto").isEqual());
        assertFalse(new EqualsBuilder2<>(e1, e3).append("toto", "titi").isEqual());
        assertFalse(new EqualsBuilder2<>(e1, e3).append("toto", "tit", e -> e.length()).append("toto", "toto").isEqual());

        try {
            new EqualsBuilder2<>(e1, e3).append(null).isEqual();
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }

        assertTrue(new EqualsBuilder2<>(e1, e2, Exception.class).append(e1.getMessage(), e2.getMessage()).isEqual());
    }
}
