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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.UUID;

import org.junit.Test;

/**
 * Check {@link HashCodeBuilder}
 *
 * @since Feb 11, 2017
 * @author Gilles
 *
 */
public class HashCodeBuilderTest {

    /**
     * Test method for
     * {@link HashCodeBuilder#append(java.lang.Object, java.util.function.Function)}.
     */
    @Test
    public void testAppendTFunctionOfTX() {
        final String uid = UUID.randomUUID().toString();
        final IllegalArgumentException e1 = new IllegalArgumentException(uid);
        final NullPointerException e2 = new NullPointerException(uid);

        assertEquals(new HashCodeBuilder().append(e1, e -> e.getMessage()).toHashCode(),
                new HashCodeBuilder().append(e2, e -> e.getMessage()).toHashCode());

        assertEquals(new HashCodeBuilder().toHashCode(), new HashCodeBuilder().append((Exception) null, e -> e.getMessage()).toHashCode());

        try {
            new HashCodeBuilder().append(e1, null).toHashCode();
            fail();
        } catch (NullPointerException e) {
            assertNotNull(e);
        }
    }
}
