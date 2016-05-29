/*
 * #%L
 * utils-web
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.web;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import fr.landel.utils.io.FileUtils;

/**
 * HTC loader tests.
 *
 * @since 11 déc. 2015
 * @author Gilles
 *
 */
public class HTCLoaderTest extends AbstractTest {

    @Autowired
    private HTCLoader htcLoader;

    /**
     * Test method for {@link fr.landel.utils.web.HTCLoader#getHTCContent()}.
     */
    @Test
    public void testGetHTCContent() {
        String content = this.htcLoader.getHTCContent();

        assertNotNull(content);

        File test = new File("target/test.htc");
        try (FileWriter fw = new FileWriter(test)) {
            fw.append(content);
        } catch (IOException e) {
            fail(e.getMessage());
        }

        assertTrue(test.isFile());

        assertTrue(FileUtils.isEqual(new File("src/main/resources/htc/PIE-1.0.0.htc"), test));
    }
}
