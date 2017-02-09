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
package fr.landel.utils.commons.exception;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

/**
 * Check abstract exception
 *
 * @since Apr 23, 2016
 * @author Gilles
 *
 */
public class AbstractExceptionTest extends AbstractException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6301742295214990685L;

    /**
     * Test method for {@link AbstractException#AbstractException()} .
     *
     * @throws AbstractException
     *             The expected exception
     */
    @Test(expected = AbstractException.class)
    public void testAbstractException() throws AbstractException {
        AbstractException exception = new AbstractException() {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractException.class));

        throw exception;
    }

    /**
     * Test method for
     * {@link AbstractException#AbstractException(java.lang.String)} .
     */
    @Test
    public void testAbstractExceptionString() {
        AbstractException exception = new AbstractException("test") {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractException.class));

        try {
            throw exception;
        } catch (AbstractException e) {
            assertEquals("test", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link AbstractException#AbstractException(java.lang.Throwable)} .
     */
    @Test
    public void testAbstractExceptionThrowable() {
        AbstractException exception = new AbstractException(new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractException.class));

        try {
            throw exception;
        } catch (AbstractException e) {
            assertNotNull(e.getCause());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link AbstractException#AbstractException(java.lang.Class, java.lang.Throwable)}
     * .
     */
    @Test
    public void testAbstractExceptionClassOfQextendsAbstractExceptionThrowable() {
        AbstractException exception = new AbstractException(AbstractExceptionTest.class, new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractException.class));

        try {
            throw exception;
        } catch (AbstractException e) {
            assertNotNull(e.getCause());
            assertEquals(AbstractExceptionTest.class.getSimpleName(), e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link AbstractException#AbstractException(java.lang.String, java.lang.Throwable)}
     * .
     */
    @Test
    public void testAbstractExceptionStringThrowable() {
        AbstractException exception = new AbstractException("test", new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractException.class));

        try {
            throw exception;
        } catch (AbstractException e) {
            assertNotNull(e.getCause());
            assertEquals("test", e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link AbstractException#AbstractException(java.lang.String, java.lang.Throwable, boolean, boolean)}
     * .
     */
    @Test
    public void testAbstractExceptionStringThrowableBooleanBoolean() {
        AbstractException exception1 = new AbstractException("test", new IOException(), false, true) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        AbstractException exception2 = new AbstractException("test", new IOException(), true, false) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractException.class));

        try {
            exception1.addSuppressed(new IOException());
            throw exception1;
        } catch (AbstractException e) {
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
        } catch (AbstractException e) {
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
