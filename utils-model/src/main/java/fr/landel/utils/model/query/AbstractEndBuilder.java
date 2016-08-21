package fr.landel.utils.model.query;

import java.io.Serializable;

import fr.landel.utils.model.AbstractEntity;

/**
 * (Description)
 *
 * @since Aug 16, 2016
 * @author Gilles
 *
 */
public abstract class AbstractEndBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>>
        extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -3257847729073920622L;

    /**
     * 
     * Constructor
     *
     * @param parentBuilder
     *            the parent builder
     * @param type
     *            the type of builder
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     */
    protected AbstractEndBuilder(final QueryBuilder<?, ?> parentBuilder, final BuilderType type, final Class<E> entityClass,
            final CharSequence alias) {
        super(parentBuilder, type, entityClass, alias);
    }

    /**
     * Group by builder.
     * 
     * @return the current query builder
     */
    public EndBuilder<E, K> groupBy() {
        final EndBuilder<E, K> endBuilder = new EndBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
        endBuilder.groupBy();
        return endBuilder;
    }

    /**
     * Group by builder.
     * 
     * @param columns
     *            The columns of the entity
     * @return the current query builder
     */
    public EndBuilder<E, K> groupBy(final CharSequence... columns) {
        final EndBuilder<E, K> endBuilder = new EndBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
        endBuilder.groupBy(columns);
        return endBuilder;
    }

    /**
     * Having builder.
     * 
     * @return the current query builder
     */
    public ClauseBuilder<E, K> having() {
        this.add(HAVING);
        return new ClauseBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
    }

    /**
     * Order by builder.
     * 
     * @param queryOrder
     *            The order queries
     * @return the current query builder
     */
    public EndBuilder<E, K> orderBy(final QueryOrder... queriesOrder) {
        final EndBuilder<E, K> endBuilder = new EndBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
        endBuilder.orderBy(queriesOrder);
        return endBuilder;
    }
}