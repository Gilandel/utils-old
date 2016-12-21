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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import fr.landel.utils.mapper.EnumMode;
import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.mapper.mappable.MappableProperty;
import fr.landel.utils.model.AbstractDTO;

/**
 * DTO parent to test mapper
 *
 * @since Dec 1, 2015
 * @author Gilles
 *
 */
@Mappable(value = EntityParent.class)
public class DTOParent extends AbstractDTO<DTOParent, String>implements CommonMethods {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -2731351091398664423L;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_LIST)
    private Byte valByte;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    @MappableProperty(mode = EnumMode.SAVE, value = PARENT_SAVE_LIST)
    private Boolean valBoolean;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Character valCharacter;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Short valShort;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Integer valInteger;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Long valLong;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Float valFloat;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Double valDouble;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS, name = "name")
    private String renamed;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    private Date date;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    @MappableProperty(mode = EnumMode.SAVE, value = PARENT_SAVE_LIST)
    private EnumLocale locale;

    @MappableProperty(mode = EnumMode.PRELOAD, value = PARENT_SAVE_LIST)
    @MappableProperty(mode = EnumMode.SAVE, value = PARENT_SAVE_LIST)
    private DTOChild child;

    @MappableProperty(mode = EnumMode.PRELOAD, value = PARENT_DETAILS)
    private Set<DTOChild> children1;

    @MappableProperty(mode = EnumMode.LOAD, value = PARENT_DETAILS)
    @MappableProperty(mode = EnumMode.SAVE, value = PARENT_SAVE_LIST)
    private List<DTOChild> children2;

    private Float onlyInDTO;

    /**
     * 
     * Constructor
     *
     */
    public DTOParent() {
        super(DTOParent.class, String.class);

        this.children1 = new HashSet<>();
        this.children2 = new ArrayList<>();
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

    /**
     * @return the children1
     */
    public Set<DTOChild> getChildren1() {
        return this.children1;
    }

    /**
     * @param children1
     *            the children1 to set
     */
    public void setChildren1(Set<DTOChild> children1) {
        this.children1 = children1;
    }

    /**
     * @return the children2
     */
    public List<DTOChild> getChildren2() {
        return this.children2;
    }

    /**
     * @param children2
     *            the children2 to set
     */
    public void setChildren2(List<DTOChild> children2) {
        this.children2 = children2;
    }

    /**
     * @return the child
     */
    public DTOChild getChild() {
        return this.child;
    }

    /**
     * @param child
     *            the child to set
     */
    public void setChild(DTOChild child) {
        this.child = child;
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
        map.put("onlyInDTO", this.onlyInDTO);
        map.put("child", this.child);
        map.put("children1", this.children1);
        map.put("children2", this.children2);
    }
}
