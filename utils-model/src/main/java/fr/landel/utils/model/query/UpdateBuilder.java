package fr.landel.utils.model.query;

import java.io.Serializable;

import fr.landel.utils.model.AbstractEntity;

public class UpdateBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 6472392322805109117L;

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
    public UpdateBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.UPDATE, entityClass, alias);
    }
}
