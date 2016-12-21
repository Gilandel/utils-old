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
package fr.landel.utils.model.mappable;

import java.util.Date;
import java.util.Map;

import fr.landel.utils.mapper.EnumMode;
import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.mapper.mappable.MappableProperty;
import fr.landel.utils.mapper.mappable.MappablesProperty;
import fr.landel.utils.model.AbstractDTO;

/**
 * DTO parent to test mapper
 *
 * @since Dec 1, 2015
 * @author Gilles
 *
 */
@Mappable(value = EntityChild.class)
public class DTOChild extends AbstractDTO<DTOChild, String>implements CommonMethods {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -2731351091398664423L;

    @MappableProperty(mode = EnumMode.LOAD, value = {CHILD_LIST, PARENT_DETAILS})
    private Byte valByte;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Boolean valBoolean;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Character valCharacter;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Short valShort;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Integer valInteger;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Long valLong;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Float valFloat;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_LIST)
    private Double valDouble;

    @MappablesProperty(name = "name", value = {@MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS),
            @MappableProperty(mode = EnumMode.SAVE, value = PARENT_SAVE_LIST)})
    private String renamed;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Date date;

    @MappableProperty(mode = EnumMode.LOAD, value = CHILD_DETAILS)
    private EnumLocale locale;

    /**
     * 
     * Constructor
     *
     */
    public DTOChild() {
        super(DTOChild.class, String.class);
    }

    @Override
    public Byte getValByte() {
        return this.valByte;
    }

    @Override
    public void setValByte(Byte valByte) {
        this.valByte = valByte;
    }

    @Override
    public Boolean getValBoolean() {
        return this.valBoolean;
    }

    @Override
    public void setValBoolean(Boolean valBoolean) {
        this.valBoolean = valBoolean;
    }

    @Override
    public Character getValCharacter() {
        return this.valCharacter;
    }

    @Override
    public void setValCharacter(Character valCharacter) {
        this.valCharacter = valCharacter;
    }

    @Override
    public Short getValShort() {
        return this.valShort;
    }

    @Override
    public void setValShort(Short valShort) {
        this.valShort = valShort;
    }

    @Override
    public Integer getValInteger() {
        return this.valInteger;
    }

    @Override
    public void setValInteger(Integer valInteger) {
        this.valInteger = valInteger;
    }

    @Override
    public Long getValLong() {
        return this.valLong;
    }

    @Override
    public void setValLong(Long valLong) {
        this.valLong = valLong;
    }

    @Override
    public Float getValFloat() {
        return this.valFloat;
    }

    @Override
    public void setValFloat(Float valFloat) {
        this.valFloat = valFloat;
    }

    @Override
    public Double getValDouble() {
        return this.valDouble;
    }

    @Override
    public void setValDouble(Double valDouble) {
        this.valDouble = valDouble;
    }

    /**
     * @return the name
     */
    public String getRenamed() {
        return this.renamed;
    }

    /**
     * @param renamed
     *            The name
     */
    public void setRenamed(String renamed) {
        this.renamed = renamed;
    }

    @Override
    public Date getDate() {
        return this.date;
    }

    @Override
    public void setDate(Date date) {
        this.date = date;
    }

    @Override
    public EnumLocale getLocale() {
        return this.locale;
    }

    @Override
    public void setLocale(EnumLocale locale) {
        this.locale = locale;
    }

    @Override
    protected void overToString(final Map<String, Object> map) {
        map.put("byte", this.valByte);
        map.put("boolean", this.valBoolean);
        map.put("character", this.valCharacter);
        map.put("short", this.valShort);
        map.put("integer", this.valInteger);
        map.put("long", this.valLong);
        map.put("float", this.valFloat);
        map.put("double", this.valDouble);
        map.put("renamed", this.renamed);
        map.put("date", this.date);
        map.put("locale", this.locale);
    }
}
