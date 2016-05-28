/*
 * #%L
 * utils-scripts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.scripts;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Enumeration of scripts
 *
 * @since 1 déc. 2015
 * @author Gilles
 *
 */
public enum EnumScripts implements ScriptsList<EnumScripts> {

    /**
     * The SQL test file (for test on loader)
     */
    TEST("test.sql"),

    /**
     * Select patient search by sector or unit (count and paginated select)
     */
    PATIENTS_SEARCH("patientsSearch.sql"),

    /**
     * Select patient search by sector or unit (count and paginated select)
     */
    INDEX_AGGS("index.elastic");

    private String name;
    private Charset charset;

    /**
     * 
     * Constructor
     *
     * @param name
     *            The file name
     * @param charset
     *            The file charset
     */
    EnumScripts(final String name, final Charset charset) {
        this.name = name;
        this.charset = charset;
    }

    /**
     * 
     * Constructor
     *
     * @param name
     *            The file name
     */
    EnumScripts(final String name) {
        this(name, StandardCharsets.UTF_8);
    }

    public String getName() {
        return this.name;
    }

    @Override
    public EnumScripts[] getValues() {
        return EnumScripts.values();
    }

    @Override
    public Charset getCharset() {
        return this.charset;
    }
}
