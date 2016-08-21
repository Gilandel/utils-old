package fr.landel.utils.model.query;

import java.io.Serializable;

import fr.landel.utils.model.AbstractEntity;

public class InsertBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 7648974302667545977L;

    /**
     * Constructor
     *
     * @param parentBuilder
     *            the parent builder
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     */
    public InsertBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.INSERT, entityClass, alias);
    }
}
