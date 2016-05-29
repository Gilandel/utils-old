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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.After;
import org.junit.Test;

import fr.landel.utils.asserts.AssertUtils;
import fr.landel.utils.io.EncodingUtils;
import fr.landel.utils.io.FileSystemUtils;
import fr.landel.utils.io.FileUtils;

/**
 * Check file utils
 *
 * @since 11 dec. 2015
 * @author Gilles Landel
 *
 */
public class FileUtilsTest {

    private static final String CHECK_CRC32_PATH = "src/test/resources/io";
    private static final String CHECK_CRC32_TARGET_PATH = "target/io";
    private static final String CHECK_CRC32_FILE = "checkCRC32.xml";

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

    private void createTargetDirectory(final String path) {
        File targetDirectory = new File(path);
        if (targetDirectory.isDirectory()) {
            assertTrue(FileSystemUtils.deleteDirectory(targetDirectory));
        }
        assertTrue(targetDirectory.mkdirs());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.io.FileUtils#getFileContent(java.io.InputStream)}
     * .
     */
    @Test
    public void testGetFileContentInputStream() {
        String test = "text";

        try (ByteArrayInputStream bais = new ByteArrayInputStream(test.getBytes(EncodingUtils.CHARSET_UTF_8))) {
            StringBuilder sb = FileUtils.getFileContent(bais);

            assertEquals(test, sb.toString());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.io.FileUtils#getFileContent(java.io.InputStream, java.nio.charset.Charset)}
     * .
     */
    @Test
    public void testGetFileContentInputStreamCharset() {
        String test = "texte accentu\u00e9";

        try (ByteArrayInputStream bais = new ByteArrayInputStream(test.getBytes(EncodingUtils.CHARSET_UTF_8))) {
            StringBuilder sb = FileUtils.getFileContent(bais, EncodingUtils.CHARSET_UTF_8);

            assertEquals(test, sb.toString());
        } catch (IOException e) {
            fail(e.getMessage());
        }

        try (ByteArrayInputStream bais = new ByteArrayInputStream(test.getBytes(EncodingUtils.CHARSET_UTF_8))) {
            StringBuilder sb = FileUtils.getFileContent(bais, EncodingUtils.CHARSET_US_ASCII);

            assertNotEquals(test, sb.toString());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link FileUtils#writeFileContent(java.lang.StringBuilder, java.io.File, java.nio.charset.Charset)}
     * .
     */
    @Test
    public void testWriteFileContentStringBuilderFileCharset() {
        StringBuilder sb = new StringBuilder();

        sb.append("toto");
        sb.append("est pr\u00eat pour partir en vacances.");
        sb.append("\n");
        sb.append("cool!");

        this.createTargetDirectory(CHECK_CRC32_TARGET_PATH);

        final File outputFile = new File(CHECK_CRC32_TARGET_PATH, "output.txt");

        try {
            FileUtils.writeFileContent(sb, outputFile, EncodingUtils.CHARSET_UTF_8);

            StringBuilder outputSb = FileUtils.getFileContent(outputFile, EncodingUtils.CHARSET_US_ASCII);

            assertNotEquals(sb.toString(), outputSb.toString());

            outputSb = FileUtils.getFileContent(outputFile, EncodingUtils.CHARSET_UTF_8);

            assertEquals(sb.toString(), outputSb.toString());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.io.FileUtils#writeFileContent(java.io.InputStream, java.lang.String)}
     * {@link fr.landel.utils.io.FileUtils#getFileContent(java.lang.String)}
     * .
     */
    @Test
    public void testWriteFileContentInputStreamString() {
        File referenceFile = new File(CHECK_CRC32_PATH, CHECK_CRC32_FILE);

        this.createTargetDirectory(CHECK_CRC32_TARGET_PATH);

        final String outputPath = CHECK_CRC32_TARGET_PATH + "/output.txt";

        try (BufferedInputStream bis = new BufferedInputStream(new FileInputStream(referenceFile))) {
            FileUtils.writeFileContent(bis, outputPath);

            StringBuilder inputSb = FileUtils.getFileContent(referenceFile.getAbsolutePath());
            StringBuilder outputSb = FileUtils.getFileContent(outputPath);

            assertEquals(inputSb.toString(), outputSb.toString());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link FileUtils#writeFileContent(java.lang.StringBuilder, java.lang.String, java.nio.charset.Charset)}
     * {@link FileUtils#getFileContent(java.lang.String, java.nio.charset.Charset)}
     * .
     */
    @Test
    public void testWriteFileContentAndReadContentCharset() {
        StringBuilder sb = new StringBuilder();

        sb.append("toto");
        sb.append("est pr\u00eat pour partir en vacances.");
        sb.append("\n");
        sb.append("cool!");

        this.createTargetDirectory(CHECK_CRC32_TARGET_PATH);

        final String outputPath = CHECK_CRC32_TARGET_PATH + "/output.txt";

        try {
            FileUtils.writeFileContent(sb, outputPath, EncodingUtils.CHARSET_UTF_8);

            StringBuilder outputSb = FileUtils.getFileContent(outputPath, EncodingUtils.CHARSET_US_ASCII);

            assertNotEquals(sb.toString(), outputSb.toString());

            outputSb = FileUtils.getFileContent(outputPath, EncodingUtils.CHARSET_UTF_8);

            assertEquals(sb.toString(), outputSb.toString());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.io.FileUtils#writeStream(java.io.InputStream, java.io.OutputStream)}
     * .
     */
    @Test
    public void testWriteStream() {
        String test = "text";

        try (ByteArrayInputStream bais = new ByteArrayInputStream(test.getBytes(EncodingUtils.CHARSET_UTF_8))) {
            try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {

                FileUtils.writeStream(bais, baos);

                assertEquals(test, baos.toString(EncodingUtils.ENCODING_UTF_8));
            }
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Test method for
     * {@link fr.landel.utils.io.FileUtils#isEqual(java.io.File, java.io.File)}
     * .
     * 
     * @throws IOException
     *             On copy failed
     */
    @Test
    public void testIsEqual() throws IOException {
        File referenceFile = new File(CHECK_CRC32_PATH, CHECK_CRC32_FILE);
        File copiedFile = new File(CHECK_CRC32_TARGET_PATH, CHECK_CRC32_FILE);

        this.createTargetDirectory(CHECK_CRC32_TARGET_PATH);

        FileSystemUtils.copyFile(referenceFile, copiedFile);

        assertTrue(FileUtils.isEqual(referenceFile, copiedFile));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.io.FileUtils#getFileContent(String, java.nio.charset.Charset, ClassLoader)}
     * .
     * 
     * @throws IOException
     *             On copy failed
     */
    @Test
    public void testGetFileContent() throws IOException {
        File referenceFile = new File(CHECK_CRC32_PATH, CHECK_CRC32_FILE);

        StringBuilder referenceContent = FileUtils.getFileContent(referenceFile, StandardCharsets.UTF_8);
        StringBuilder content = FileUtils.getFileContent("io/" + CHECK_CRC32_FILE, StandardCharsets.UTF_8, null);

        assertNotNull(referenceContent);
        assertNotNull(content);

        AssertUtils.isEqual(referenceContent, content);
    }
}
