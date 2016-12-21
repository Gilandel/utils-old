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

import javax.persistence.Transient;

/**
 * Abstract embeddable entity.
 *
 * @since Jul 13, 2015
 * @author Gilles
 * 
 * @param <E>
 *            The embeddable.
 * @param <E1>
 *            The first join entity.
 * @param <K1>
 *            The first foreign key type.
 * @param <E2>
 *            The second join entity.
 * @param <K2>
 *            The second foreign key type.
 */
public abstract class AbstractEmbeddableEntity<
/**/E extends AbstractEmbeddableEntity<E, E1, K1, E2, K2>,
/**/E1 extends AbstractEntity<E1, K1>,
/**/K1 extends Serializable & Comparable<K1>,
/**/E2 extends AbstractEntity<E2, K2>,
/**/K2 extends Serializable & Comparable<K2>> implements Serializable, Comparable<E> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -2860006982766576382L;

    /**
     * @return The first entity
     */
    @Transient
    public abstract E1 getEntity1();

    /**
     * @return The second entity
     */
    @Transient
    public abstract E2 getEntity2();
}
