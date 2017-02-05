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
package fr.landel.utils.model;

import java.util.HashMap;
import java.util.Map;

import fr.landel.utils.commons.over.AbstractOverObject;

/**
 * Abstract class to force implementation of Object methods (replace
 * StringBuilder from overToString by a map).
 *
 * @since Jul 14, 2015
 * @author Gilles
 * 
 * @param <O>
 *            The over object type.
 */
public abstract class AbstractModelOverObject<O extends AbstractModelOverObject<O>> extends AbstractOverObject<O> {
    /**
     * 
     * Constructor
     *
     */
    public AbstractModelOverObject() {
        super();
    }

    /**
     * Constructor.
     *
     * @param clazz
     *            The over class.
     */
    public AbstractModelOverObject(final Class<O> clazz) {
        super(clazz);
    }

    /**
     * To force implementation of toString.
     * 
     * @param map
     *            The map of properties.
     */
    protected abstract void overToString(final Map<String, Object> map);

    @Override
    protected void overToString(final StringBuilder sb) {
        final Map<String, Object> map = new HashMap<>();
        this.overToString(map);
        AbstractOverObject.mapToString(sb, map);
    }
}
