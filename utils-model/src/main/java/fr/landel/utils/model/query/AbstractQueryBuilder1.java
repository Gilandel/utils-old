/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.query;

import java.util.ArrayList;

import fr.landel.utils.commons.EnumChar;
import fr.landel.utils.commons.StringUtils;

/**
 * The abstract query builder
 *
 * @since Nov 30, 2015
 * @author Gilles
 *
 */
public abstract class AbstractQueryBuilder1 extends ArrayList<CharSequence> {

    /**
     * The opened parenthesis character
     */
    public static final String PARENTHESIS_OPEN = EnumChar.PARENTHESIS_OPEN.getUnicode();

    /**
     * The closed parenthesis character
     */
    public static final String PARENTHESIS_CLOSE = EnumChar.PARENTHESIS_CLOSE.getUnicode();

    /**
     * The space character
     */
    public static final String SPACE = EnumChar.SPACE.getUnicode();

    /**
     * The 'all' character
     */
    public static final String ALL = EnumChar.ASTERISK.getUnicode();

    /**
     * The equal operator
     */
    protected static final String EQUALS = EnumChar.EQUALS.getUnicode();

    /**
     * The colon character
     */
    protected static final char COLON = EnumChar.COLON.getCharacter();

    /**
     * The distinct key word
     */
    protected static final String DISTINCT = "DISTINCT";

    /**
     * Serial
     */
    private static final long serialVersionUID = 629818342327662943L;

    /**
     * Build the query
     * 
     * @return the query {@code String}
     */
    public String build() {
        return StringUtils.join(this, SPACE);
    }

    @Override
    public String toString() {
        return this.build();
    }
}
