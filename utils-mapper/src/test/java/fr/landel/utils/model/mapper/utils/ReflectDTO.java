package fr.landel.utils.model.mapper.utils;

import java.util.Collection;

import fr.landel.utils.model.mapper.EnumMode;
import fr.landel.utils.model.mapper.mappable.Mappable;
import fr.landel.utils.model.mapper.mappable.MappableProperty;
import fr.landel.utils.model.mapper.mappable.MappablesProperty;

/**
 * 
 * Class to check reflection utils
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
@Mappable({Class.class, Collection.class})
public class ReflectDTO {

    public static final EnumMode MODE = EnumMode.DEFAULT;
    protected static final short SHORT_NUMBER = 0;
    private static final byte BYTE_NUMBER = 0;
    private static final transient int INT_TRANSIENT = 0;
    public double doubleNumber;

    @MappableProperty
    protected boolean bool;
    protected transient long longNumber = INT_TRANSIENT;
    private final String string = null;

    @MappablesProperty({@MappableProperty, @MappableProperty})
    private transient Object object = "";
    private volatile long longVolatile = BYTE_NUMBER;

    /**
     * @return the bool
     */
    public final boolean isBool() {
        return this.bool;
    }

    /**
     * @param bool
     *            the bool to set
     */
    public final void setBool(boolean bool) {
        this.bool = bool;
    }

    /**
     * @return the object
     */
    public final Object getObject() {
        return this.object;
    }

    /**
     * @param object
     *            the object to set
     */
    public final void setObject(Object object) {
        this.object = object;
    }

    /**
     * @return the longVolatile
     */
    @MappableProperty
    public final long getLongVolatile() {
        return this.longVolatile;
    }

    /**
     * @param longVolatile
     *            the longVolatile to set
     */
    public final void setLongVolatile(long longVolatile) {
        this.longVolatile = longVolatile;
    }

    /**
     * @return the string
     */
    public final String getString() {
        return this.string;
    }
}