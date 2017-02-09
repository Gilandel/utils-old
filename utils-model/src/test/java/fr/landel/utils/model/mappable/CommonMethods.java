/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.mappable;

import java.util.Date;

import fr.landel.utils.model.mapper.MyDTOIdentifier;

/**
 * Common methods (Parent vs Child and DTO vs Entity)
 *
 * @since Dec 1, 2015
 * @author Gilles
 *
 */
public interface CommonMethods extends MyDTOIdentifier {

    /**
     * @return the valByte
     */
    Byte getValByte();

    /**
     * @param valByte
     *            the valByte to set
     */
    void setValByte(Byte valByte);

    /**
     * @return the valBoolean
     */
    Boolean getValBoolean();

    /**
     * @param valBoolean
     *            the valBoolean to set
     */
    void setValBoolean(Boolean valBoolean);

    /**
     * @return the valCharacter
     */
    Character getValCharacter();

    /**
     * @param valCharacter
     *            the valCharacter to set
     */
    void setValCharacter(Character valCharacter);

    /**
     * @return the valShort
     */
    Short getValShort();

    /**
     * @param valShort
     *            the valShort to set
     */
    void setValShort(Short valShort);

    /**
     * @return the valInteger
     */
    Integer getValInteger();

    /**
     * @param valInteger
     *            the valInteger to set
     */
    void setValInteger(Integer valInteger);

    /**
     * @return the valLong
     */
    Long getValLong();

    /**
     * @param valLong
     *            the valLong to set
     */
    void setValLong(Long valLong);

    /**
     * @return the valFloat
     */
    Float getValFloat();

    /**
     * @param valFloat
     *            the valFloat to set
     */
    void setValFloat(Float valFloat);

    /**
     * @return the valDouble
     */
    Double getValDouble();

    /**
     * @param valDouble
     *            the valDouble to set
     */
    void setValDouble(Double valDouble);

    /**
     * @return the date
     */
    Date getDate();

    /**
     * @param date
     *            the date to set
     */
    void setDate(Date date);

    /**
     * @return the locale
     */
    EnumLocale getLocale();

    /**
     * @param locale
     *            the locale to set
     */
    void setLocale(EnumLocale locale);
}
