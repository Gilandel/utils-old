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
 * ToString JSON style
 *
 * @since Mar 5, 2017
 * @author Gilles
 *
 */
public class ToStringStyleJSONSpaced extends AbstractToStringStyle {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -1671814171209301362L;

    private static final String EQUALS = ": ";
    private static final String SEP = ", ";

    @Override
    protected String getStart() {
        return BRACE_OPEN;
    }

    @Override
    protected String getTitleStart() {
        return EMPTY;
    }

    @Override
    protected String getTitleEnd() {
        return EMPTY;
    }

    @Override
    protected String getTitleSeparator() {
        return EQUALS;
    }

    @Override
    protected String getPropertiesStart() {
        return BRACE_OPEN;
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
        return BRACE_CLOSE;
    }

    @Override
    protected String getEnd() {
        return BRACE_CLOSE;
    }
}
