/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.builder;

/**
 * ToString default style
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public class ToStringStyleDefault extends AbstractToStringStyle {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -1671814171209301363L;

    private static final String EMPTY = "";
    private static final String EQUALS = "=";
    private static final String START = "[";
    private static final String END = "]";
    private static final String SEP = ",";

    @Override
    protected String getTitleStart() {
        return EMPTY;
    }

    @Override
    protected String getTitleEnd() {
        return EMPTY;
    }

    @Override
    protected String getPropertiesStart() {
        return START;
    }

    @Override
    protected String getKeyStart() {
        return EMPTY;
    }

    @Override
    protected String getKeyEnd() {
        return EMPTY;
    }

    @Override
    protected String getPropertySeparator() {
        return EQUALS;
    }

    @Override
    protected String getValueStart() {
        return EMPTY;
    }

    @Override
    protected String getValueEnd() {
        return EMPTY;
    }

    @Override
    protected String getPropertiesSeparator() {
        return SEP;
    }

    @Override
    protected String getPropertiesEnd() {
        return END;
    }
}
