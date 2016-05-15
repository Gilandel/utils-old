/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model;

/**
 * Context constants
 *
 * @since 9 févr. 2016
 * @author Gilles
 *
 */
public interface ContextConstants {

    /**
     * Mapper context
     */
    String CONTEXT_MAPPER = "classpath:applicationContext-mapper.xml";

    /**
     * SQL scripts loader context
     */
    String CONTEXT_SQL_SCRIPTS_LOADER = "classpath:applicationContext-sqlScriptsLoader.xml";

    /**
     * Constants interface
     */
    void voidMethod();
}
