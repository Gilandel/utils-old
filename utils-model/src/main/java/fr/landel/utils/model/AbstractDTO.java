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

/**
 * Abstract DTO.
 *
 * @since Jul 14, 2015
 * @author Gilles
 * 
 * @param <D>
 *            The DTO type.
 * @param <K>
 *            The primary key type.
 */
public abstract class AbstractDTO<D extends AbstractDTO<D, K>, K extends Serializable> extends AbstractModelOverObject<D>
        implements IDO<D, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -7966490818333185949L;

    private Class<K> primaryKeyClass;

    /**
     * The primary key
     */
    private K primaryKey;

    /**
     * The loaded state
     */
    private Boolean loaded;

    /**
     * 
     * Constructor
     *
     */
    public AbstractDTO() {
        super();
    }

    /**
     * Constructor
     *
     * @param clazz
     *            The DTO class.
     * @param primaryKeyClass
     *            The primary key class.
     */
    public AbstractDTO(final Class<D> clazz, final Class<K> primaryKeyClass) {
        super(clazz);

        this.primaryKeyClass = primaryKeyClass;
        this.loaded = Boolean.FALSE;
    }

    /**
     * @return the primaryKeyClass
     */
    public Class<K> getPrimaryKeyClass() {
        return this.primaryKeyClass;
    }

    /**
     * @param primaryKeyClass
     *            the primaryKeyClass to set
     */
    public void setPrimaryKeyClass(Class<K> primaryKeyClass) {
        this.primaryKeyClass = primaryKeyClass;
    }

    /**
     * @return the loaded
     */
    public Boolean getLoaded() {
        return this.loaded;
    }

    /**
     * @param loaded
     *            the loaded to set
     */
    public void setLoaded(Boolean loaded) {
        this.loaded = loaded;
    }

    /**
     * @return the primary key
     */
    public K getPrimaryKey() {
        return this.primaryKey;
    }

    /**
     * @param primaryKey
     *            the primary key to set
     */
    public void setPrimaryKey(K primaryKey) {
        this.primaryKey = primaryKey;
    }

    @Override
    protected boolean overEquals(D obj) {
        return this.getPrimaryKey().equals(obj.getPrimaryKey());
    }

    @Override
    protected int overHashCode() {
        return Objects.hashCode(this.getPrimaryKey());
    }
}
