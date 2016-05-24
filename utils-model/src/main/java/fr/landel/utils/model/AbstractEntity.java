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

import java.io.Serializable;
import java.util.Objects;

import javax.persistence.Transient;

/**
 * Abstract entity.
 *
 * @since 13 juil. 2015
 * @author Gilles
 *
 * @param <E>
 *            The entity
 * @param <K>
 *            The primary key type
 */
public abstract class AbstractEntity<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends
        AbstractModelOverComparable<E> implements IDO<E, K> {

    /**
     * The length of URL's fields
     * 
     * @see <a href="http://support.microsoft.com/kb/208427/EN-US">Maximum URL
     *      length in IE</a>
     */
    protected static final int URL_LENGTH = 2083;

    /**
     * The length of encrypted fields in SHA-256
     */
    protected static final int SHA_256_LENGTH = 64;

    /**
     * The length of encrypted fields in BCrypt
     */
    protected static final int BCRYPT_LENGTH = 60;

    /**
     * The length of enum's fields
     */
    protected static final int ENUM_LENGTH = 31;

    /**
     * Serial
     */
    private static final long serialVersionUID = -7287960064101334368L;

    /**
     * 
     * Constructor
     *
     */
    public AbstractEntity() {
        super(null);
    }

    /**
     * Constructor
     *
     * @param clazz
     *            The entity class.
     */
    public AbstractEntity(final Class<E> clazz) {
        super(clazz);
    }

    /**
     * @return The primary key
     */
    @Override
    @Transient
    public abstract K getPrimaryKey();

    /**
     * @param key
     *            The primary key
     */
    @Transient
    public abstract void setPrimaryKey(K key);

    @Override
    protected int overCompareTo(final E obj) {
        if (this.getPrimaryKey() != null && obj.getPrimaryKey() != null) {
            return this.getPrimaryKey().compareTo(obj.getPrimaryKey());
        }
        return -1;
    }

    @Override
    protected boolean overEquals(E obj) {
        if (this.getPrimaryKey() != null && obj.getPrimaryKey() != null) {
            return this.getPrimaryKey().equals(obj.getPrimaryKey());
        }
        return false;
    }

    @Override
    protected int overHashCode() {
        return Objects.hashCode(this.getPrimaryKey());
    }
}
