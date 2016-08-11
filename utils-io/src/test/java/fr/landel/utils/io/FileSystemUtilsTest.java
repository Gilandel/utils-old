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
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Test;

import fr.landel.utils.assertor.expect.Expect;

/**
 * Check utility class (files).
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public class FileSystemUtilsTest {

    private static final String XML_EXT = "xml";
    private static final String TXT_EXT = "txt";
    private static final FilenameFilter XML_FILTER = FileSystemUtils.createFilenameFilter(XML_EXT);
    private static final FileFilter TXT_FILTER = (file) -> TXT_EXT.equals(FileSystemUtils.getExtensionPart(file));

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
     * Check {@link FileSystemUtils#copyDirectory}
     */
    @Test
    public void testCopyDirectory() {
        try {
            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH, XML_FILTER);
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_TARGET_PATH, XML_FILTER));

            String dest = "target/dir" + UUID.randomUUID();
            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, dest, TXT_FILTER);
            assertTrue(new File(dest).isDirectory());

            FileSystemUtils.copyDirectory(new File(CHECK_CRC32_PATH), new File("target/dir" + UUID.randomUUID()));
            FileSystemUtils.copyDirectory(new File(CHECK_CRC32_PATH), new File("target/dir" + UUID.randomUUID()), XML_FILTER);
            FileSystemUtils.copyDirectory(new File(CHECK_CRC32_PATH), new File("target/dir" + UUID.randomUUID()), TXT_FILTER);

            FileSystemUtils.copyDirectory(new File(CHECK_CRC32_TARGET_PATH), new File("target/dir" + UUID.randomUUID()));

            String newDir = "target/dir" + UUID.randomUUID();
            if (FileSystemUtils.createDirectory(newDir)) {
                FileSystemUtils.copyDirectory(new File(newDir), new File("target/dir" + UUID.randomUUID()));
            }
        } catch (IOException e) {
            fail(e.getMessage());
        }

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory(null, (String) null);
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory(null, "");
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory("", null);
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory(null, (File) null);
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory(null, new File(""));
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory(new File(""), null);
            fail();
        }, FileNotFoundException.class);
    }

    /**
     * Check {@link FileSystemUtils#moveDirectory}
     */
    @Test
    public void testMoveDirectory() {
        try {
            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH, XML_FILTER);
            assertEquals(CHECK_CRC32_VALUE, FileCRC32Utils.getCRC32(CHECK_CRC32_TARGET_PATH, XML_FILTER));
            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH + 2, TXT_FILTER);

            String dest = "target/dir" + UUID.randomUUID();
            FileSystemUtils.moveDirectory(CHECK_CRC32_TARGET_PATH, dest);
            assertFalse(new File(CHECK_CRC32_TARGET_PATH).isDirectory());
            assertTrue(new File(dest).isDirectory());

            String dest2 = "target/dir" + UUID.randomUUID();
            FileSystemUtils.moveDirectory(new File(dest), new File(dest2));

            FileSystemUtils.moveDirectory(new File(dest2), new File(dest), XML_FILTER);
            FileSystemUtils.moveDirectory(dest, dest2, XML_FILTER);

            FileSystemUtils.moveDirectory(new File(CHECK_CRC32_TARGET_PATH + 2), new File(dest), TXT_FILTER);
            FileSystemUtils.moveDirectory(dest, dest2, TXT_FILTER);

            FileSystemUtils.copyDirectory(CHECK_CRC32_TARGET_PATH, "target/dir" + UUID.randomUUID());
            FileSystemUtils.moveDirectory(CHECK_CRC32_TARGET_PATH, "target/dir" + UUID.randomUUID());

            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH + 4, XML_FILTER);
            FileSystemUtils.copyDirectory(CHECK_CRC32_TARGET_PATH + 4 + "/checkCRC32.xml", CHECK_CRC32_PATH);
        } catch (IOException e) {
            fail(e.getMessage());
        }

        Expect.exception(() -> {
            FileSystemUtils.copyDirectory(CHECK_CRC32_PATH, CHECK_CRC32_TARGET_PATH, XML_FILTER);
            File file = new File(CHECK_CRC32_TARGET_PATH, "checkCRC32.xml");
            assertTrue(file.isFile());
            try (FileReader reader = new FileReader(file)) {
                FileSystemUtils.moveDirectory(new File(CHECK_CRC32_TARGET_PATH), new File(CHECK_CRC32_TARGET_PATH + 3));
            }
            fail();
        }, IOException.class);
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
     * Check {@link FileSystemUtils#createDirectory}
     * 
     * @throws IOException
     */
    @Test
    public void testCreateDirectory() throws IOException {

        UUID uuid = UUID.randomUUID();
        // new directory
        assertTrue(FileSystemUtils.createDirectory("target/createa" + uuid));
        // already exists
        assertTrue(FileSystemUtils.createDirectory("target/createa" + uuid));
        // from file
        assertTrue(FileSystemUtils.createDirectory(new File("target/create" + UUID.randomUUID())));

        // path is a file
        FileSystemUtils.copyFile(CHECK_CRC32_FILE, CHECK_CRC32_TARGET_PATH + "output.file");
        assertFalse(FileSystemUtils.createDirectory(CHECK_CRC32_TARGET_PATH + "output.file"));

        // null
        assertFalse(FileSystemUtils.createDirectory((String) null));
        assertFalse(FileSystemUtils.createDirectory((File) null));
    }

    /**
     * Check {@link FileSystemUtils#moveFile}
     * 
     * @throws IOException
     */
    @Test
    public void testMoveFile() throws IOException {
        // prepare
        FileSystemUtils.copyFile(CHECK_CRC32_FILE, CHECK_CRC32_TARGET_PATH + "output2.file");

        FileSystemUtils.moveFile(CHECK_CRC32_TARGET_PATH + "output2.file", CHECK_CRC32_TARGET_PATH + "/output3.file");
        File file = new File(CHECK_CRC32_TARGET_PATH, "output4.file");
        FileSystemUtils.moveFile(new File(CHECK_CRC32_TARGET_PATH, "output3.file"), file);

        // same file
        FileSystemUtils.moveFile(file, file);

        Expect.exception(() -> {
            FileSystemUtils.moveFile(new File(CHECK_CRC32_TARGET_PATH, "output2.file"), new File(CHECK_CRC32_TARGET_PATH, "output3.file"));
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.moveFile(null, CHECK_CRC32_TARGET_PATH + "output3.file");
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.moveFile((String) null, null);
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.moveFile(CHECK_CRC32_TARGET_PATH + "output3.file", null);
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            try (FileReader reader = new FileReader(file)) {
                FileSystemUtils.moveFile(file, new File(CHECK_CRC32_TARGET_PATH + "output5.file"));
            }
            fail();
        }, IOException.class);

        Expect.exception(() -> {
            FileSystemUtils.moveFile(CHECK_CRC32_TARGET_PATH + "/output4.file", "//\\");
            fail();
        }, FileNotFoundException.class);

        Expect.exception(() -> {
            FileSystemUtils.moveFile(file, new File("//\\"));
            fail();
        }, FileNotFoundException.class);
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
