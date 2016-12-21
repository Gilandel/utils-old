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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * Check {@link Constants}
 *
 * @since Aug 3, 2016
 * @author Gilles
 *
 */
public class ConstantsTest extends AbstractTest {

    /**
     * Test method for {@link Constants#getProperty} .
     */
    @Test
    public void testGetProperty() {
        assertEquals(DEFAULT_ASSERTION, Constants.getProperty(null, ""));
        assertEquals("the boolean should be true", Constants.getProperty("boolean.true"));
        assertEquals("the boolean should be true", Constants.getProperty("boolean.true", "arg"));
        assertEquals("the object '{0}' should be null", Constants.getProperty("object.null"));
        assertEquals("the object 'arg' should be null", Constants.getProperty("object.null", "arg"));
        assertEquals("the object 'arg' should be null", Constants.getProperty("object.null", "arg", ""));
    }
}
