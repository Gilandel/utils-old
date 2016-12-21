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
 * Abstract class to force implementation of Comparable methods.
 *
 * @since Jul 14, 2015
 * @author Gilles
 * 
 * @param <O>
 *            The over object type.
 */
public abstract class AbstractModelOverComparable<O extends AbstractModelOverComparable<O>> extends AbstractModelOverObject<O> implements
        Comparable<O> {

    /**
     * Constructor.
     *
     */
    public AbstractModelOverComparable() {
        super(null);
    }

    /**
     * Constructor
     *
     * @param clazz
     *            The over class.
     */
    public AbstractModelOverComparable(final Class<O> clazz) {
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
