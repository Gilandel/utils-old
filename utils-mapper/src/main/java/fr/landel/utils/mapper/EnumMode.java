/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.mapper;

/**
 * Mapping mode
 *
 * @since Mar 17, 2016
 * @author Gilles
 *
 */
public enum EnumMode {

    /**
     * In preload mode (map an Entity into a DTO, but only the identifier
     * property (primaryKey))
     */
    PRELOAD(0),

    /**
     * In load mode (map an Entity into a DTO)
     */
    LOAD(1),

    /**
     * In save mode (map a DTO into an Entity)
     */
    SAVE(2),

    /**
     * Default mode and bidirectional (if preload, load or save mode are
     * specified, this mode is ignored)
     */
    DEFAULT(-1);

    private int order;

    private EnumMode(final int order) {
        this.order = order;
    }

    public int getOrder() {
        return this.order;
    }
}
