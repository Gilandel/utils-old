/*
 * #%L
 * utils-poi
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.poi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.junit.Test;

import fr.landel.utils.io.FileSystemUtils;

/**
 * Check the XLS comparison
 *
 * @since 10 déc. 2015
 * @author Gilles
 *
 */
public class AssertXLSTest {

    private static final String SRC_DIR = "src/test/resources";

    /**
     * Check XLS comparison
     * 
     * @throws IOException
     *             On loading exception
     */
    @Test
    public void testAssertEquals() throws IOException {
        final String fileName = "file1.xls";

        final File source = new File(SRC_DIR);
        final File target = new File("target/xls");

        File expectedFile = new File(source, fileName);
        File targetFile = new File(target, fileName);

        if (target.isDirectory()) {
            assertTrue(FileSystemUtils.deleteDirectory(target));
        }
        assertTrue(target.mkdirs());

        FileSystemUtils.copyFile(expectedFile, targetFile);

        AssertXLS.assertEquals(expectedFile, targetFile);
    }

    /**
     * Check the raised exception on comparison error
     * 
     * @throws IOException
     *             On loading exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testAssertNotEquals() throws IOException {
        final File source = new File(SRC_DIR);

        File expectedFile = new File(source, "file1.xls");
        File targetFile = new File(source, "file2.xls"); // Selected cell

        AssertXLS.assertEquals(expectedFile, targetFile);
    }

    /**
     * Check XLS comparison exception
     * 
     * @throws IOException
     *             On loading exception
     */
    @Test
    public void testAssertNotEquals2() throws IOException {
        final File source = new File(SRC_DIR);

        File expectedFile = new File(source, "file1.xls"); // cell color: red
        File targetFile = new File(source, "file3.xls"); // cell color: violet

        try {
            AssertXLS.assertEquals(expectedFile, targetFile);

            fail("The files must have differences");
        } catch (IllegalArgumentException e) {
            assertEquals("Style fill foreground color [3, 4]", e.getMessage());
        }
    }
}
