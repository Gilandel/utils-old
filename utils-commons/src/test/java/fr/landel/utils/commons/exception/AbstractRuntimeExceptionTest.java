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
 * @since Apr 23, 2016
 * @author Gilles
 *
 */
public class AbstractRuntimeExceptionTest extends AbstractRuntimeException {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6301742225214990685L;

    /**
     * Test method for
     * {@link AbstractRuntimeException#AbstractRuntimeException()} .
     *
     * @throws AbstractRuntimeException
     *             The expected exception
     */
    @Test(expected = AbstractRuntimeException.class)
    public void testAbstractRuntimeException() throws AbstractRuntimeException {
        AbstractRuntimeException exception = new AbstractRuntimeException() {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractRuntimeException.class));

        throw exception;
    }

    /**
     * Test method for
     * {@link AbstractRuntimeException#AbstractRuntimeException(java.lang.String)}
     * .
     */
    @Test
    public void testAbstractRuntimeExceptionString() {
        AbstractRuntimeException exception = new AbstractRuntimeException("test") {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractRuntimeException.class));

        try {
            throw exception;
        } catch (AbstractRuntimeException e) {
            assertEquals("test", e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link AbstractRuntimeException#AbstractRuntimeException(java.lang.Throwable)}
     * .
     */
    @Test
    public void testAbstractRuntimeExceptionThrowable() {
        AbstractRuntimeException exception = new AbstractRuntimeException(new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractRuntimeException.class));

        try {
            throw exception;
        } catch (AbstractRuntimeException e) {
            assertNotNull(e.getCause());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link AbstractRuntimeException#AbstractRuntimeException(java.lang.Class, java.lang.Throwable)}
     * .
     */
    @Test
    public void testAbstractRuntimeExceptionClassOfQextendsAbstractRuntimeExceptionThrowable() {
        AbstractRuntimeException exception = new AbstractRuntimeException(AbstractRuntimeExceptionTest.class, new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractRuntimeException.class));

        try {
            throw exception;
        } catch (AbstractRuntimeException e) {
            assertNotNull(e.getCause());
            assertEquals(AbstractRuntimeExceptionTest.class.getSimpleName(), e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link AbstractRuntimeException#AbstractRuntimeException(java.lang.String, java.lang.Throwable)}
     * .
     */
    @Test
    public void testAbstractRuntimeExceptionStringThrowable() {
        AbstractRuntimeException exception = new AbstractRuntimeException("test", new IOException()) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractRuntimeException.class));

        try {
            throw exception;
        } catch (AbstractRuntimeException e) {
            assertNotNull(e.getCause());
            assertEquals("test", e.getMessage());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }

    /**
     * Test method for
     * {@link AbstractRuntimeException#AbstractRuntimeException(java.lang.String, java.lang.Throwable, boolean, boolean)}
     * .
     */
    @Test
    public void testAbstractRuntimeExceptionStringThrowableBooleanBoolean() {
        AbstractRuntimeException exception1 = new AbstractRuntimeException("test", new IOException(), false, true) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        AbstractRuntimeException exception2 = new AbstractRuntimeException("test", new IOException(), true, false) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(AbstractRuntimeException.class));

        try {
            exception1.addSuppressed(new IOException());
            throw exception1;
        } catch (AbstractRuntimeException e) {
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
        } catch (AbstractRuntimeException e) {
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
