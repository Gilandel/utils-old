package fr.landel.utils.model.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.commons.CollectionUtils2;
import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.commons.exception.IllegalOperationException;
import fr.landel.utils.model.AbstractEntity;

/**
 * (Description)
 *
 * @since Aug 16, 2016
 * @author Gilles
 *
 */
public class EndBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> extends AbstractBuilder<E, K> {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -7305380285385724578L;

    private List<CharSequence> groupByBuilder;
    private List<CharSequence> orderByBuilder;
    private ClauseBuilder<E, K> havingBuilder;

    /**
     * Constructor
     *
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     */
    public EndBuilder(final QueryBuilder<?, ?> parentBuilder, final Class<E> entityClass, final CharSequence alias) {
        super(parentBuilder, BuilderType.END, entityClass, alias);
        parentBuilder.setEnd(this);

        this.groupByBuilder = new ArrayList<>();
        this.orderByBuilder = new ArrayList<>();
    }

    /**
     * Group by builder.
     * 
     * @return the current query builder
     */
    public EndBuilder<E, K> groupBy() {
        return this.groupBy(this.getAlias());
    }

    /**
     * Group by builder.
     * 
     * @param columns
     *            The columns of the entity
     * @return the current query builder
     */
    public EndBuilder<E, K> groupBy(final CharSequence... columns) {
        this.groupByBuilder.addAll(Arrays.asList(columns));

        return this;
    }

    /**
     * Having builder.
     * 
     * @return the current query builder
     */
    public ClauseBuilder<E, K> having() {
        Assertor.that(this.groupByBuilder).isNotEmpty()
                .toThrow(() -> new IllegalOperationException("'having' cannot be used without 'group by' clause"));

        this.havingBuilder = new ClauseBuilder<>(this.getParentBuilder(), this.getEntityClass(), this.getAlias());
        return this.havingBuilder;
    }

    /**
     * Order by builder.
     * 
     * @param queryOrder
     *            The order queries
     * @return the current query builder
     */
    public EndBuilder<E, K> orderBy(final QueryOrder... queriesOrder) {
        this.orderByBuilder.addAll(CollectionUtils2.transformIntoList(queriesOrder));

        return this;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String build() {
        final List<CharSequence> builder = new ArrayList<>();
        if (!this.groupByBuilder.isEmpty()) {
            builder.add(GROUP_BY);
            builder.add(StringUtils.join(this.groupByBuilder, StringUtils.JOIN_SEPARATOR));
        }
        if (this.havingBuilder != null) {
            builder.add(HAVING);
            builder.add(this.havingBuilder.build());
        }
        if (!this.orderByBuilder.isEmpty()) {
            builder.add(ORDER_BY);
            builder.add(StringUtils.join(this.orderByBuilder, StringUtils.JOIN_SEPARATOR));
        }
        return StringUtils.join(builder, SPACE);
    }
}
