package fr.landel.utils.commons.builder;

import java.util.function.Supplier;

/**
 * {@link ToStringBuilder} style enumeration
 *
 * @since Mar 11, 2017
 * @author Gilles
 *
 */
public enum ToStringStyles {

    /**
     * The default toString style.
     * 
     * <pre>
     * test[java.awt.Color[r=0,g=0,b=0],0,blue=java.awt.Color[r=0,g=0,b=255],value=1 153 120 156,569,supplier,SUPPLIER,supplier=12,supplier=SUPPLIER,optional,OPTIONAL,optional=optional,optional=OPTIONAL]
     * </pre>
     */
    DEFAULT(ToStringStyleDefault::new),

    /**
     * The JSON toString style
     * 
     * <pre>
     * {test:{java.awt.Color[r=0,g=0,b=0],0,blue:java.awt.Color[r=0,g=0,b=255],value:1 153 120 156,569,supplier,SUPPLIER,supplier:12,supplier:SUPPLIER,optional,OPTIONAL,optional:optional,optional:OPTIONAL}}
     * </pre>
     */
    JSON(ToStringStyleJSON::new),

    /**
     * The JSON toString style with spaces
     * 
     * <pre>
     * {test: {java.awt.Color[r=0,g=0,b=0], 0, blue: java.awt.Color[r=0,g=0,b=255], value: 1 153 120 156, 569, supplier, SUPPLIER, supplier: 12, supplier: SUPPLIER, optional, OPTIONAL, optional: optional, optional: OPTIONAL}}
     * </pre>
     */
    JSON_SPACED(ToStringStyleJSONSpaced::new),

    /**
     * The JSON toString style with quotes
     * 
     * <pre>
     * 
     * </pre>
     */
    JSON_QUOTED(ToStringStyleJSONQuoted::new),

    /**
     * The readable toString style
     * 
     * <pre>
     * test = 
     * ['java.awt.Color[r=0,g=0,b=0]',
     * '0',
     * 'blue' = 'java.awt.Color[r=0,g=0,b=255]',
     * 'value' = '1 153 120 156,569',
     * 'supplier',
     * 'SUPPLIER',
     * 'supplier' = '12',
     * 'supplier' = 'SUPPLIER',
     * 'optional',
     * 'OPTIONAL',
     * 'optional' = 'optional',
     * 'optional' = 'OPTIONAL']
     * </pre>
     */
    READABLE(ToStringStyleReadable::new),

    /**
     * The JSON toString style
     * 
     * <pre>
     * (test: (java.awt.Color[r=0,g=0,b=0], 0, blue: java.awt.Color[r=0,g=0,b=255], value: 1 153 120 156, 569, supplier, SUPPLIER, supplier: 12, supplier: SUPPLIER, optional, OPTIONAL, optional: optional, optional: OPTIONAL))
     * </pre>
     */
    PARENTHESIS(ToStringStyleParenthesis::new);

    private Supplier<ToStringStyle> supplier;

    ToStringStyles(final Supplier<ToStringStyle> supplier) {
        this.supplier = supplier;
    }

    /**
     * @return the constructor supplier
     */
    public Supplier<ToStringStyle> getSupplier() {
        return this.supplier;
    }
}
