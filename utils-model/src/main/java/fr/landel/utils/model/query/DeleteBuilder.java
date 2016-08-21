package fr.landel.utils.model.query;

import java.io.Serializable;

import fr.landel.utils.model.AbstractEntity;

public class DeleteBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 2648541528971563915L;

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
    public DeleteBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.DELETE, entityClass, alias);
    }
}
