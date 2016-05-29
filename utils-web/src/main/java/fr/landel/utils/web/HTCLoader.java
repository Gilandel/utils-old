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

import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import fr.landel.utils.io.EncodingUtils;
import fr.landel.utils.io.FileUtils;

/**
 * PIE HTC loader.
 *
 * @since 11 déc. 2015
 * @author Gilles
 *
 */
@Component
public final class HTCLoader {

    private static final Logger LOGGER = LoggerFactory.getLogger(HTCLoader.class);

    private static final String PIE_HTC_PATH = "htc/PIE-1.0.0.htc";

    private StringBuilder content;

    /**
     * The constructor
     */
    public HTCLoader() {
        this.content = new StringBuilder();

        this.init();
    }

    private void init() {
        try (InputStream is = HTCLoader.class.getClassLoader().getResourceAsStream(PIE_HTC_PATH)) {
            this.content.append(FileUtils.getFileContent(is, EncodingUtils.CHARSET_UTF_8));
        } catch (IOException e) {
            LOGGER.error("Error occurs while loading " + PIE_HTC_PATH, e);
        }
    }

    /**
     * Load the HTC file (PIE-1.0.0.htc)
     * 
     * @return The content of the file
     */
    public String getHTCContent() {
        return this.content.toString();
    }
}
