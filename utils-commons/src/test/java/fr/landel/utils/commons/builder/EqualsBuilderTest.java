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

import java.util.function.BiPredicate;
import java.util.function.Function;

import org.junit.Test;

/**
 * Check {@link EqualsBuilder}
 *
 * @since Feb 10, 2017
 * @author Gilles
 *
 */
public class EqualsBuilderTest {

    /**
     * Test method for
     * {@link EqualsBuilder#append(java.lang.Object, java.lang.Object, java.util.function.Function)}.
     */
    @Test
    public void testAppend() {
        final IllegalArgumentException e1 = new IllegalArgumentException("error");
        final NullPointerException e2 = new NullPointerException("error");
        final IllegalArgumentException e3 = new IllegalArgumentException("ERROR");
        final IllegalArgumentException e4 = new IllegalArgumentException((String) null);

        final Function<Exception, String> getter = e -> e.getMessage();
        final BiPredicate<String, String> predicate = (a, b) -> a.equalsIgnoreCase(b);

        assertTrue(new EqualsBuilder().append(e1, e2, getter).isEquals());
        assertFalse(new EqualsBuilder().append(e1, e3, getter).isEquals());

        assertFalse(new EqualsBuilder().append(e1, e3, getter).append(e1, e2, getter).isEquals());
        assertFalse(new EqualsBuilder().append(e1, e2, getter).append(e1, e3, getter).isEquals());

        assertTrue(new EqualsBuilder().append((Exception) null, null, getter).isEquals());
        assertFalse(new EqualsBuilder().append(e1, null, getter).isEquals());
        assertFalse(new EqualsBuilder().append(null, e2, getter).isEquals());

        assertFalse(new EqualsBuilder().append(e1, e4, getter, predicate).isEquals());
        assertFalse(new EqualsBuilder().append(e4, e1, getter, predicate).isEquals());
        assertTrue(new EqualsBuilder().append(e4, e4, getter, predicate).isEquals());

        try {
            new EqualsBuilder().append(e1, e2, null).isEquals();
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }
}
