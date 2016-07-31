/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.tuple;

/**
 * <p>
 * A quad consisting of four elements of same type.
 * </p>
 * 
 * <p>
 * This class is an abstract implementation defining the basic API. It refers to
 * the elements as 'first', 'second', 'third' and 'fourth'.
 * </p>
 * 
 * <p>
 * Subclass implementations may be mutable or immutable. However, there is no
 * restriction on the type of the stored objects that may be stored. If mutable
 * objects are stored in the triple, then the triple itself effectively becomes
 * mutable.
 * </p>
 *
 * @param <T>
 *            the element type
 *
 * @since 26 juil. 2016
 * @author Gilles
 *
 */
public abstract class Quadri<T> extends Quad<T, T, T, T> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -6033257956595424676L;
}