/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.commons.io;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.commons.io.CloseableManager;
import fr.landel.utils.commons.io.StreamUtils;

/**
 * Check utility class (streams).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class CloseableManagerTest {

    private static final String CHECK_CRC32_PATH = "src/test/resources/io";
    private static final String CHECK_CRC32_FILE_INPUT = CHECK_CRC32_PATH + "/checkCRC32.xml";

    /**
     * Test method for {@link CloseableManager#isCloseable(java.lang.Class)} .
     */
    @Test
    public void testStreamClass() {
        FileInputStream fis;
        try {
            fis = new FileInputStream(CHECK_CRC32_FILE_INPUT);

            CloseableManager.addCloseable(CloseableManagerTest.class, fis);

            assertTrue(CloseableManager.isCloseable(CloseableManagerTest.class));

            CloseableManager.close(CloseableManagerTest.class);

            assertFalse(CloseableManager.isCloseable(CloseableManagerTest.class));
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for {@link CloseableManager#isCloseable(java.lang.String)} .
     */
    @Test
    public void testStreamString() {
        try {
            StreamUtils.createBufferedInputStream(CHECK_CRC32_FILE_INPUT);

            assertTrue(CloseableManager.isCloseable(CHECK_CRC32_FILE_INPUT));

            CloseableManager.close(CHECK_CRC32_FILE_INPUT);

            assertFalse(CloseableManager.isCloseable(CHECK_CRC32_FILE_INPUT));
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for {@link CloseableManager#isCloseable(java.io.File)} .
     */
    @Test
    public void testStreamFile() {
        try {
            File file = new File(CHECK_CRC32_FILE_INPUT);

            StreamUtils.createBufferedInputStream(file);

            assertTrue(CloseableManager.isCloseable(file));

            CloseableManager.close(file);

            assertFalse(CloseableManager.isCloseable(file));
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link CloseableManager#addCloseable(java.lang.String, java.io.Closeable)}
     * .
     */
    @Test
    public void testAddStreamStringCloseable() {
        FileInputStream fis;
        try {
            fis = new FileInputStream(CHECK_CRC32_FILE_INPUT);

            CloseableManager.addCloseable(CHECK_CRC32_FILE_INPUT, fis);

            assertTrue(CloseableManager.isCloseable(CHECK_CRC32_FILE_INPUT));

            CloseableManager.close(CHECK_CRC32_FILE_INPUT);

            assertFalse(CloseableManager.isCloseable(CHECK_CRC32_FILE_INPUT));
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link CloseableManager#addCloseable(java.io.File, java.io.Closeable)} .
     */
    @Test
    public void testAddStreamFileCloseable() {
        FileInputStream fis;
        try {
            File file = new File(CHECK_CRC32_FILE_INPUT);

            fis = new FileInputStream(file);

            CloseableManager.addCloseable(file, fis);

            assertTrue(CloseableManager.isCloseable(file));

            CloseableManager.close(file);

            assertFalse(CloseableManager.isCloseable(file));
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for {@link CloseableManager#closeAll()}.
     */
    @Test
    public void testCloseAll() {
        try {
            StreamUtils.createBufferedInputStream(CHECK_CRC32_FILE_INPUT);

            assertTrue(CloseableManager.isCloseable(CHECK_CRC32_FILE_INPUT));

            CloseableManager.closeAll();

            assertFalse(CloseableManager.isCloseable(CHECK_CRC32_FILE_INPUT));
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for {@link CloseableManager#close(java.io.Closeable)} .
     */
    @Test
    public void testCloseCloseable() {
        FileInputStream fis;
        try {
            fis = new FileInputStream(CHECK_CRC32_FILE_INPUT);

            CloseableManager.close(fis);

            fis.available();

            fail("error has to be thrown");
        } catch (FileNotFoundException e) {
            fail(e.getMessage());
        } catch (IOException e) {
            assertNotNull(e);
        }
    }
}
