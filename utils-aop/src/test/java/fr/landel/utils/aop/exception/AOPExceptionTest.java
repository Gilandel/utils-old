/*
 * #%L
 * utils-aop
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.aop.exception;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

import fr.landel.utils.aop.exception.AOPException;

/**
 * Check AOP exception
 *
 * @since 2 déc. 2015
 * @author Gilles
 *
 */
public class AOPExceptionTest {

    /**
     * 
     * Constructor
     *
     */
    public AOPExceptionTest() {
    }

    /**
     * Test method for
     * {@link fr.landel.utils.aop.exception.AOPException#AOPException()}
     * .
     */
    @Test
    public void testAOPException() {
        AOPException exception = new AOPException();
        assertNull(exception.getMessage());
        assertNull(exception.getCause());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.aop.exception.AOPException#AOPException(java.lang.String)}
     * .
     */
    @Test
    public void testAOPExceptionString() {
        final String expectedMessage = "message";
        AOPException exception = new AOPException(expectedMessage);
        assertEquals(expectedMessage, exception.getMessage());
        assertNull(exception.getCause());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.aop.exception.AOPException#AOPException(java.lang.Throwable)}
     * .
     */
    @Test
    public void testAOPExceptionThrowable() {
        final Throwable expectedCause = new IllegalArgumentException("illegal");
        AOPException exception = new AOPException(expectedCause);
        assertEquals(AOPException.class.getSimpleName(), exception.getMessage());
        assertEquals(expectedCause, exception.getCause());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.aop.exception.AOPException#AOPException(java.lang.String, java.lang.Throwable)}
     * .
     */
    @Test
    public void testAOPExceptionStringThrowable() {
        final String expectedMessage = "message";
        final Throwable expectedCause = new IllegalArgumentException("illegal");
        AOPException exception = new AOPException(expectedMessage, expectedCause);
        assertEquals(expectedMessage, exception.getMessage());
        assertEquals(expectedCause, exception.getCause());
    }

}
