/*
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
package fr.landel.utils.commons.over;

/**
 * Abstract class to force implementation of Comparable methods.
 *
 * @since Jul 14, 2015
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
