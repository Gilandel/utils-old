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
package org.gl.utils.commons.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Locale;

import org.junit.After;
import org.junit.Test;

/**
 * Check utility class (files).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class FileSizeUtilsTest {

    private static final String CHECK_CRC32_PATH = "src/test/resources/io";
    private static final String CHECK_CRC32_TARGET_PATH = "target/io";
    private static final String CHECK_CRC32_FILE = CHECK_CRC32_PATH + "/checkCRC32.xml";
    private static final long CHECK_CRC32_SIZE = 1_099L;

    /**
     * Remove test directory
     */
    @After
    public void dispose() {
        File target = new File(CHECK_CRC32_TARGET_PATH);

        if (target.isDirectory()) {
            assertTrue(FileSystemUtils.deleteDirectory(target));
        }
    }

    /**
     * Check size formatter
     */
    @Test
    public void testFormatSize() {
        assertEquals("1 Octet", FileSizeUtils.formatSize(1L));
        assertEquals("23 Octets", FileSizeUtils.formatSize(23L));
        assertEquals("23.017 Kio", FileSizeUtils.formatSize(23569L, Locale.US));
        assertEquals("22.478 Mio", FileSizeUtils.formatSize(23569896L, Locale.US));
        assertEquals("21.951 Gio", FileSizeUtils.formatSize(23569896548L, Locale.US));
        assertEquals("21.437 Tio", FileSizeUtils.formatSize(23569896548855L, Locale.US));
        assertEquals("20.934 Pio", FileSizeUtils.formatSize(23569896548855142L, Locale.US));
        assertEquals("20,934 Pio", FileSizeUtils.formatSize(23569896548855142L, Locale.FRANCE));
    }

    /**
     * Check size getter
     * 
     * @throws IOException
     *             On copy failed
     */
    @Test
    public void testGetSize() throws IOException {
        assertEquals(CHECK_CRC32_SIZE, FileSizeUtils.getSize(CHECK_CRC32_FILE));

        File target = new File(CHECK_CRC32_TARGET_PATH, "tree");

        if (target.isDirectory()) {
            assertTrue(FileSystemUtils.deleteDirectory(target));
        }

        assertTrue(target.mkdirs());
        assertTrue(target.isDirectory());

        FileSystemUtils.copyFile(CHECK_CRC32_FILE, target.getAbsolutePath());
        File target2 = new File(target, "sub");
        assertTrue(target2.mkdirs());
        assertTrue(target2.isDirectory());

        FileSystemUtils.copyFile(CHECK_CRC32_FILE, target2.getAbsolutePath());

        assertEquals(CHECK_CRC32_SIZE * 2, FileSizeUtils.getSize(target));
    }
}
