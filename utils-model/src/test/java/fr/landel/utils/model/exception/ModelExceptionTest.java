/*-
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.exception;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

/**
 * (Description)
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
public class ModelExceptionTest {

    /**
     * Test method for {@link ModelException#ModelException()}.
     * 
     * @throws ModelException
     *             the expected exception
     */
    @Test(expected = ModelException.class)
    public void testModelException() throws ModelException {
        ModelException exception = new ModelException() {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(ModelException.class));

        throw exception;
    }

    /**
     * Test method for {@link ModelException#ModelException(java.lang.String)}.
     */
    @Test
    public void testModelExceptionString() {
        ModelException exception = new ModelException("test") {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(ModelException.class));

        try {
            throw exception;
        } catch (ModelException e) {
            assertEquals("test", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link ModelException#ModelException(java.lang.Throwable)}.
     */
    @Test
    public void testModelExceptionThrowable() {
        ModelException exception = new ModelException(new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(ModelException.class));

        try {
            throw exception;
        } catch (ModelException e) {
            assertNotNull(e.getCause());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link ModelException#ModelException(java.lang.String, java.lang.Throwable)}.
     */
    @Test
    public void testModelExceptionStringThrowable() {
        ModelException exception = new ModelException("test", new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(ModelException.class));

        try {
            throw exception;
        } catch (ModelException e) {
            assertNotNull(e.getCause());
            assertEquals("test", e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }
}
