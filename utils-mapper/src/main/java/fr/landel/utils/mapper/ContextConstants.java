/*-
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */

package fr.landel.utils.mapper;

/**
 * Context constants
 *
 * @since Aug 12, 2016
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
