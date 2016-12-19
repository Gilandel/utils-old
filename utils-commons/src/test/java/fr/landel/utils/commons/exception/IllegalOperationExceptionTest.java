/*
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
package fr.landel.utils.commons.exception;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

/**
 * Check abstract runtime exception
 *
 * @since 23 avr. 2016
 * @author Gilles
 *
 */
public class IllegalOperationExceptionTest extends IllegalOperationException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6301742225214990685L;

    /**
     * Test method for
     * {@link IllegalOperationException#IllegalOperationException()} .
     *
     * @throws IllegalOperationException
     *             The expected exception
     */
    @Test(expected = IllegalOperationException.class)
    public void testIllegalOperationException() throws IllegalOperationException {
        IllegalOperationException exception = new IllegalOperationException() {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(IllegalOperationException.class));

        throw exception;
    }

    /**
     * Test method for
     * {@link IllegalOperationException#IllegalOperationException(java.lang.String)}
     * .
     */
    @Test
    public void testIllegalOperationExceptionString() {
        IllegalOperationException exception = new IllegalOperationException("test") {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(IllegalOperationException.class));

        try {
            throw exception;
        } catch (IllegalOperationException e) {
            assertEquals("test", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link IllegalOperationException#IllegalOperationException(java.lang.Throwable)}
     * .
     */
    @Test
    public void testIllegalOperationExceptionThrowable() {
        IllegalOperationException exception = new IllegalOperationException(new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(IllegalOperationException.class));

        try {
            throw exception;
        } catch (IllegalOperationException e) {
            assertNotNull(e.getCause());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link IllegalOperationException#IllegalOperationException(java.lang.Class, java.lang.Throwable)}
     * .
     */
    @Test
    public void testIllegalOperationExceptionClassOfQextendsIllegalOperationExceptionThrowable() {
        IllegalOperationException exception = new IllegalOperationException(IllegalOperationExceptionTest.class, new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(IllegalOperationException.class));

        try {
            throw exception;
        } catch (IllegalOperationException e) {
            assertNotNull(e.getCause());
            assertEquals(IllegalOperationExceptionTest.class.getSimpleName(), e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link IllegalOperationException#IllegalOperationException(java.lang.String, java.lang.Throwable)}
     * .
     */
    @Test
    public void testIllegalOperationExceptionStringThrowable() {
        IllegalOperationException exception = new IllegalOperationException("test", new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(IllegalOperationException.class));

        try {
            throw exception;
        } catch (IllegalOperationException e) {
            assertNotNull(e.getCause());
            assertEquals("test", e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link IllegalOperationException#IllegalOperationException(java.lang.String, java.lang.Throwable, boolean, boolean)}
     * .
     */
    @Test
    public void testIllegalOperationExceptionStringThrowableBooleanBoolean() {
        IllegalOperationException exception1 = new IllegalOperationException("test", new IOException(), false, true) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        IllegalOperationException exception2 = new IllegalOperationException("test", new IOException(), true, false) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(IllegalOperationException.class));

        try {
            exception1.addSuppressed(new IOException());
            throw exception1;
        } catch (IllegalOperationException e) {
            assertNotNull(e.getCause());
            assertEquals("test", e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
            assertNotNull(e.getSuppressed());
            assertEquals(0, e.getSuppressed().length);
            assertNotNull(e.getStackTrace());
            assertTrue(e.getStackTrace().length > 0);
        }

        try {
            exception2.addSuppressed(new IOException());
            throw exception2;
        } catch (IllegalOperationException e) {
            assertNotNull(e.getCause());
            assertEquals("test", e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
            assertNotNull(e.getSuppressed());
            assertTrue(e.getSuppressed().length > 0);
            assertNotNull(e.getStackTrace());
            assertEquals(0, e.getStackTrace().length);
        }
    }
}
