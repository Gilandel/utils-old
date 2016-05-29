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
package fr.landel.utils.scripts;

import java.nio.charset.Charset;

/**
 * The list of scripts loaded
 *
 * @since 1 déc. 2015
 * @author Gilles
 * 
 * @param <E>
 *            The extended type
 */
public interface ScriptsList<E extends ScriptsList<E>> {

    /**
     * @return The complete list of elements
     */
    E[] getValues();

    /**
     * @return The element name
     */
    String getName();

    /**
     * @return The charset
     */
    Charset getCharset();
}
