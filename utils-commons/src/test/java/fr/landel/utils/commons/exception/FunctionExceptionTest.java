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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

/**
 * Check function exception
 *
 * @since Apr 23, 2016
 * @author Gilles
 *
 */
public class FunctionExceptionTest {

    /**
     * Test method for
     * {@link FunctionException#FunctionException(java.lang.Throwable)} .
     */
    @Test
    public void testFunctionExceptionThrowable() {
        FunctionException exception = new FunctionException(new IOException("test")) {

            /**
             * serialVersionUID
             */
            private static final long serialVersionUID = 1L;
        };

        assertTrue(Exception.class.isAssignableFrom(FunctionException.class));

        try {
            throw exception;
        } catch (FunctionException e) {
            assertNotNull(e.getCause());
            assertTrue(IOException.class.isAssignableFrom(e.getCause().getClass()));
        }
    }
}
