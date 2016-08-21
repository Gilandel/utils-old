package fr.landel.utils.model.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import fr.landel.utils.commons.EnumChar;
import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.model.AbstractEntity;

public abstract class AbstractBuilder<E extends AbstractEntity<E, K>, K extends Serializable & Comparable<K>> implements Serializable {

    /**
     * The opened parenthesis character
     */
    public static final String PARENTHESIS_OPEN = EnumChar.PARENTHESIS_OPEN.getUnicode();

    /**
     * The closed parenthesis character
     */
    public static final String PARENTHESIS_CLOSE = EnumChar.PARENTHESIS_CLOSE.getUnicode();

    /**
     * The space character
     */
    public static final String SPACE = EnumChar.SPACE.getUnicode();

    /**
     * The colon character
     */
    protected static final char COLON = EnumChar.COLON.getCharacter();

    /**
     * Alias keyword
     */
    protected static final String AS = "AS";

    /**
     * OR operator
     */
    public static final String OR = "OR";

    /**
     * AND operator
     */
    public static final String AND = "AND";

    /**
     * Where clause
     */
    public static final String WHERE = "WHERE";

    /**
     * Comma character
     */
    protected static final String COMMA = EnumChar.COMMA.getUnicode();

    /**
     * Select keyword
     */
    protected static final String SELECT = "SELECT";

    /**
     * Update keyword
     */
    protected static final String UPDATE = "UPDATE";

    /**
     * Insert keyword
     */
    protected static final String INSERT = "INSERT INTO";

    /**
     * Delete keyword
     */
    protected static final String DELETE = "DELETE FROM";

    /**
     * From keyword
     */
    protected static final String FROM = "FROM";

    /**
     * Set keyword (update)
     */
    protected static final String SET = "SET";

    /**
     * Left keyword (join)
     */
    protected static final String LEFT = "LEFT";

    /**
     * Right keyword (join)
     */
    protected static final String RIGHT = "RIGHT";

    /**
     * Outer keyword (join)
     */
    protected static final String OUTER = "OUTER";

    /**
     * inner keyword (join)
     */
    protected static final String INNER = "INNER";

    /**
     * Join keyword (join)
     */
    protected static final String JOIN = "JOIN";

    /**
     * Group by keyword
     */
    protected static final String GROUP_BY = "GROUP BY";

    /**
     * Having by keyword
     */
    protected static final String HAVING = "HAVING";

    /**
     * Order by keyword
     */
    protected static final String ORDER_BY = "ORDER BY";

    /**
     * Limit keyword (delimiter)
     */
    protected static final String LIMIT = "LIMIT";

    /**
     * Offset keyword (delimiter)
     */
    protected static final String OFFSET = "OFFSET";

    /**
     * Count keyword (select)
     */
    protected static final String COUNT = "COUNT";

    /**
     * List of authorized characters (sorted by their ASCII value for binary
     * search function)
     */
    protected static final char[] AUTHORIZED_CHARACTERS = StringUtils
            .toChars("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz");

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 513885337529035759L;

    private final QueryBuilder<?, ?> parentBuilder;
    private final BuilderType type;
    private final Class<E> entityClass;
    private final CharSequence alias;
    private final List<CharSequence> builder;

    /**
     * 
     * Constructor
     *
     * @param entityClass
     *            the entity class
     * @param alias
     *            the alias
     */
    protected AbstractBuilder(final Class<E> entityClass, final CharSequence alias) {
        this.parentBuilder = null;
        this.type = BuilderType.QUERY;
        this.entityClass = entityClass;
        this.builder = new ArrayList<>();
        this.alias = StringUtils.defaultIfEmpty(alias, this.getAlias());
    }

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
    protected AbstractBuilder(final QueryBuilder<?, ?> parentBuilder, final BuilderType type, final Class<E> entityClass,
            final CharSequence alias) {
        this.parentBuilder = parentBuilder;
        this.type = type;
        this.entityClass = entityClass;
        this.alias = alias;
        this.builder = new ArrayList<>();
    }

    /**
     * @return the parentBuilder
     */
    protected QueryBuilder<?, ?> getParentBuilder() {
        return this.parentBuilder;
    }

    /**
     * @return the type
     */
    protected BuilderType getType() {
        return this.type;
    }

    /**
     * @return the entityClass
     */
    protected Class<E> getEntityClass() {
        return this.entityClass;
    }

    /**
     * @return the alias
     */
    protected CharSequence getAlias() {
        return this.alias;
    }

    /**
     * @return the builder
     */
    protected List<CharSequence> getBuilder() {
        return this.builder;
    }

    /**
     * Add a char sequence to the builder
     * 
     * @param sequence
     *            the sequence to add
     * @return true, if added
     */
    protected boolean add(final CharSequence sequence) {
        return this.builder.add(sequence);
    }

    /**
     * Add a char sequence to the builder
     * 
     * @param sequence
     *            the sequence to add
     * @return true, if added
     */
    protected boolean add(final AbstractBuilder<?, ?> builder) {
        return this.builder.addAll(builder.builder);
    }

    /**
     * Add all char sequences to the builder
     * 
     * @param sequences
     *            the sequences to add
     * @return true, if added
     */
    protected boolean addAll(final List<CharSequence> sequences) {
        return this.builder.addAll(sequences);
    }

    /**
     * Add builder.
     * 
     * @param query
     *            The sub-query
     * @param <T>
     *            the entity class of the sub-query
     * @param <Y>
     *            the primary key type
     * @return the current query builder
     */
    protected <T extends AbstractEntity<T, Y>, Y extends Serializable & Comparable<Y>> boolean add(final QueryBuilder<T, Y> query) {
        if (query != null) {
            return this.add(PARENTHESIS_OPEN) && this.add(query) && this.add(PARENTHESIS_CLOSE);
        }

        return false;
    }

    /**
     * @return build the expression
     */
    protected String build() {
        return StringUtils.join(this.builder, SPACE);
    }

    /**
     * Generates an alias
     * 
     * @param clazz
     *            the class linked to the alias
     * @return the alias
     */
    protected static final String generateAlias(final Class<?> clazz) {
        return new StringBuilder(clazz.getSimpleName().toLowerCase()).append(StringUtils.remove(UUID.randomUUID().toString(), '-'))
                .toString();
    }

    @Override
    public String toString() {
        return this.parentBuilder.build();
    }
}