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

import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.annotation.XmlTransient;

import com.fasterxml.jackson.annotation.JsonIgnore;

import fr.landel.utils.commons.CastUtils;

/**
 * Abstract class to force implementation of Object methods.
 *
 * @since Jul 14, 2015
 * @author Gilles Landel
 * 
 * @param <O>
 *            The over object type.
 */
public abstract class AbstractOverObject<O extends AbstractOverObject<O>> {

    private Class<O> clazz;

    /**
     * Default constructor (mainly for de-serialization).
     */
    public AbstractOverObject() {
    }

    /**
     * Constructor.
     *
     * @param clazz
     *            The over class.
     */
    public AbstractOverObject(final Class<O> clazz) {
        this();

        this.clazz = clazz;
    }

    /**
     * Get the class.
     * 
     * @return the clazz
     */
    @XmlTransient
    @JsonIgnore
    public Class<O> getOverClass() {
        return this.clazz;
    }

    /**
     * To force implementation of toString.
     * 
     * @param sb
     *            The string builder.
     */
    protected abstract void overToString(StringBuilder sb);

    /**
     * To force implementation of equals.
     * 
     * @param obj
     *            The object to check.
     * @return If it's the same.
     */
    protected abstract boolean overEquals(O obj);

    /**
     * To force implementation of hashCode.
     * 
     * @return The hash code.
     */
    protected abstract int overHashCode();

    /**
     * Injects and formats the properties and append them into the string
     * builder.
     * 
     * @param sb
     *            The string builder.
     * @param map
     *            The map of properties.
     */
    protected static void mapToString(final StringBuilder sb, final Map<String, Object> map) {
        for (Entry<String, Object> entry : map.entrySet()) {
            sb.append(entry.getKey()).append("=").append(entry.getValue()).append(", ");
        }
        sb.delete(sb.length() - 2, sb.length());
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        if (this.getOverClass() != null) {
            sb.append(this.getOverClass().getSimpleName());
        } else {
            sb.append(this.getClass().getSimpleName());
        }
        sb.append(" [");
        this.overToString(sb);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !this.getOverClass().isAssignableFrom(obj.getClass())) {
            return false;
        }
        return this.overEquals(CastUtils.cast(obj, this.getOverClass()));
    }

    @Override
    public int hashCode() {
        return this.overHashCode();
    }
}
