/*
 * #%L
 * utils-io
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Test;

/**
 * Check utility class (files).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class FileSystemUtilsTest {

    private static final String XML_EXT = "xml";
    private static final FilenameFilter XML_FILTER = FileSystemUtils.createFilenameFilter(XML_EXT);

    private static final String CHECK_CRC32_PATH = "src/test/resources/io";
    private static final String CHECK_CRC32_TARGET_PATH = "target/io";
    private static final String CHECK_CRC32_FILE = CHECK_CRC32_PATH + "/checkCRC32.xml";
    private static final Long CHECK_CRC32_VALUE = 3_893_630_386L;
    private static final Long CHECK_CRC32_DIR_UNIX_VALUE = 3_440_695_467L;
    private static final Long CHECK_CRC32_DIR_WIN_VALUE = 580_225_974L;

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
     * Check if all special characters are correctly replaced
     */
    @Test
    public void testReplaceSpecialCharacters() {
        String result = FileSystemUtils.replaceSpecialCharacters("Je\\|:pars/-<en>?*vacances\"", "-");
        assertEquals("Je---pars---en---vacances-", result);

        result = FileSystemUtils.replaceSpecialCharacters("Je\\|:pars/-<en>?*vacances\"", null);
        assertEquals("Jepars-envacances", result);
    }

    /**
     * Check the extension getter
     */
    @Test
    public void testGetExtension() {
        assertEquals(XML_EXT, FileSystemUtils.getExtensionPart(CHECK_CRC32_FILE));
    }

    /**
     * Check the CRC32 getter
     */
    @Test
    public void testGetCRC32() {
        try {
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_FILE));
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(new File(CHECK_CRC32_FILE)));
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(StreamUtils.createBufferedInputStream(CHECK_CRC32_FILE)));

            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_PATH, XML_FILTER));

            final long crc = FileCRC32Utils.getCRC32(CHECK_CRC32_PATH);
            assertTrue(crc == CHECK_CRC32_DIR_WIN_VALUE || crc == CHECK_CRC32_DIR_UNIX_VALUE);

            final File emptyDir = new File("target/empty");
            assertTrue(FileSystemUtils.createDirectory(emptyDir));
            assertEquals(Long.valueOf(0L), FileCRC32Utils.getCRC32(emptyDir));

            final File unknownDir = new File("target/unknown");

            assertEquals(Long.valueOf(0L), FileCRC32Utils.getCRC32(unknownDir));
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Check the copy directory method with filter
     */
    @Test
    public void testCopyDirectory() {
        try {
            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH, XML_FILTER);

            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_TARGET_PATH, XML_FILTER));
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Check the remove directory method
     */
    @Test
    public void testRemoveDirectory() {
        try {
            final File target = new File(CHECK_CRC32_TARGET_PATH);

            if (!target.exists()) {
                FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH, XML_FILTER);
            }

            if (target.exists()) {
                FileSystemUtils.deleteDirectory(CHECK_CRC32_TARGET_PATH);

                assertFalse(target.exists());
            } else {
                fail("The target '" + target.getAbsolutePath() + "' doesn't exists");
            }
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Check {@link FileSystemUtils#getExtensionPart}
     */
    @Test
    public void testGetExtensionPart() {
        assertEquals("log", FileSystemUtils.getExtensionPart(new File("target/file.log")));
        assertEquals("log", FileSystemUtils.getExtensionPart("target/sub\\file.log"));
        assertEquals("", FileSystemUtils.getExtensionPart("target/"));
        assertEquals("log", FileSystemUtils.getExtensionPart("file.log"));
        assertEquals("", FileSystemUtils.getExtensionPart("file."));
        assertEquals("", FileSystemUtils.getExtensionPart("file"));
        assertNull(FileSystemUtils.getExtensionPart((File) null));
        assertNull(FileSystemUtils.getExtensionPart((String) null));
    }

    /**
     * Check {@link FileSystemUtils#getFileNamePart}
     */
    @Test
    public void testGetFileNamePart() {
        assertEquals("file", FileSystemUtils.getFileNamePart(new File("target/file.log")));
        assertEquals("file", FileSystemUtils.getFileNamePart("target/sub\\file.log"));
        assertEquals("", FileSystemUtils.getFileNamePart("target/"));
        assertEquals("file", FileSystemUtils.getFileNamePart("file.log"));
        assertEquals("file", FileSystemUtils.getFileNamePart("file."));
        assertEquals("file", FileSystemUtils.getFileNamePart("file"));
        assertNull(FileSystemUtils.getFileNamePart((File) null));
        assertNull(FileSystemUtils.getFileNamePart((String) null));
    }

    /**
     * Check {@link FileSystemUtils#createFile}
     */
    @Test
    public void testCreateFile() {
        File file = FileSystemUtils.createFile("target", "classes", "fr", "landel", "utils", "io", "FileSystemUtils.class");

        String expected = StringUtils.join(Arrays.asList("target", "classes", "fr", "landel", "utils", "io", "FileSystemUtils.class"),
                File.separator);

        assertEquals(expected, file.getPath());

        assertNull(FileSystemUtils.createFile((File) null, "classes", "fr", "landel", "utils", "io", "FileSystemUtils.class"));
        assertNull(FileSystemUtils.createFile("target"));
    }
}
