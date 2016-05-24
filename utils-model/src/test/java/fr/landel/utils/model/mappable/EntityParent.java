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

import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.model.AbstractEntity;

/**
 * Entity parent to test mapper.
 *
 * @since 27 nov. 2015
 * @author Gilles
 *
 */
@Mappable(value = DTOParent.class)
public class EntityParent extends AbstractEntity<EntityParent, String> implements CommonMethods {

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

    private EntityChild child;

    private Set<EntityChild> children1;

    private List<EntityChild> children2;

    private Long onlyInEntity;

    /**
     * 
     * Constructor
     *
     */
    public EntityParent() {
        super(EntityParent.class);

        this.children1 = new HashSet<>();
        this.children2 = new ArrayList<>();
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

    /**
     * @return the child
     */
    public EntityChild getChild() {
        return this.child;
    }

    /**
     * @param child
     *            the child to set
     */
    public void setChild(EntityChild child) {
        this.child = child;
    }

    /**
     * @return the children1
     */
    public Set<EntityChild> getChildren1() {
        return this.children1;
    }

    /**
     * @param children1
     *            the children1 to set
     */
    public void setChildren1(Set<EntityChild> children1) {
        this.children1 = children1;
    }

    /**
     * @return the children2
     */
    public List<EntityChild> getChildren2() {
        return this.children2;
    }

    /**
     * @param children2
     *            the children2 to set
     */
    public void setChildren2(List<EntityChild> children2) {
        this.children2 = children2;
    }

    /**
     * @return the onlyInEntity
     */
    public Long getOnlyInEntity() {
        return this.onlyInEntity;
    }

    /**
     * @param onlyInEntity
     *            the onlyInEntity to set
     */
    public void setOnlyInEntity(Long onlyInEntity) {
        this.onlyInEntity = onlyInEntity;
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
        map.put("onlyInEntity", this.onlyInEntity);
        map.put("child", this.child);
        map.put("children1", this.children1);
        map.put("children2", this.children2);
    }
}
