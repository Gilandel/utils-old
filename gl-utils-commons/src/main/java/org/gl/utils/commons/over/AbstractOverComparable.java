/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * 
 * This code is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons.over;

/**
 * Abstract class to force implementation of Comparable methods.
 *
 * @since 14 juil. 2015
 * @author Gilles Landel
 * 
 * @param <O>
 *            The over object type.
 */
public abstract class AbstractOverComparable<O extends AbstractOverComparable<O>> extends AbstractOverObject<O> implements Comparable<O> {

    /**
     * Default constructor (mainly for de-serialization).
     */
    public AbstractOverComparable() {
        super(null);
    }

    /**
     * Constructor
     *
     * @param clazz
     *            The over class.
     */
    public AbstractOverComparable(final Class<O> clazz) {
        super(clazz);
    }

    /**
     * To force implementation of compareTo.
     * 
     * @param obj
     *            The object.
     * @return The compare to result.
     */
    protected abstract int overCompareTo(O obj);

    @Override
    public int compareTo(O obj) {
        if (obj == null) {
            return -1;
        }
        return this.overCompareTo(obj);
    }
}
