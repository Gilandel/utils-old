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

import fr.landel.utils.model.AbstractEntity;
import fr.landel.utils.model.mapper.mappable.Mappable;

/**
 * Entity child to test mapper.
 *
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
@Mappable(value = DTOChild.class)
public class EntityChild extends AbstractEntity<EntityChild, String> implements CommonMethods {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 3596881374018184721L;

    private String pk;

    private Byte valByte;

    private Boolean valBoolean;

    private Character valCharacter;

    private Short valShort;

    private Integer valInteger;

    private Long valLong;

    private Float valFloat;

    private Double valDouble;

    private String name;

    private Date date;

    private EnumLocale locale;

    /**
     * 
     * Constructor
     *
     */
    public EntityChild() {
        super(EntityChild.class);
    }

    /**
     * @return the pk
     */
    public String getPk() {
        return this.pk;
    }

    /**
     * @param pk
     *            the pk to set
     */
    public void setPk(String pk) {
        this.pk = pk;
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
    public String getName() {
        return this.name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
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
    public String getPrimaryKey() {
        return this.pk;
    }

    @Override
    public void setPrimaryKey(String key) {
        this.pk = key;
    }

    @Override
    protected void overToString(final Map<String, Object> map) {
        map.put("pk", this.pk);
        map.put("byte", this.valByte);
        map.put("boolean", this.valBoolean);
        map.put("character", this.valCharacter);
        map.put("short", this.valShort);
        map.put("integer", this.valInteger);
        map.put("long", this.valLong);
        map.put("float", this.valFloat);
        map.put("double", this.valDouble);
        map.put("name", this.name);
        map.put("date", this.date);
        map.put("locale", this.locale);
    }
}
