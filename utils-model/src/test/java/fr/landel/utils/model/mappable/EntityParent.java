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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import org.hibernate.annotations.GenericGenerator;

import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.model.AbstractEntity;

/**
 * Entity parent to test mapper.
 *
 * @since Nov 27, 2015
 * @author Gilles
 *
 */
@Entity
@Mappable(value = DTOParent.class)
public class EntityParent extends AbstractEntity<EntityParent, String> implements CommonMethods {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = 3596881374018184721L;

    @Id
    @GeneratedValue(generator = "uuid2")
    @GenericGenerator(name = "uuid2", strategy = "uuid2")
    @Column(name = "id", length = 50)
    private String pk;

    @Column(name = "val_byte")
    private Byte valByte;

    @Column(name = "val_boolean")
    private Boolean valBoolean;

    @Column(name = "val_character")
    private Character valCharacter;

    @Column(name = "val_short")
    private Short valShort;

    @Column(name = "val_integer")
    private Integer valInteger;

    @Column(name = "val_long")
    private Long valLong;

    @Column(name = "val_float")
    private Float valFloat;

    @Column(name = "val_double")
    private Double valDouble;

    @Column(name = "name")
    private String name;

    @Column(name = "date_")
    private Date date;

    @Column(name = "locale")
    @Enumerated(EnumType.STRING)
    private EnumLocale locale;

    @OneToOne
    @JoinColumn(name = "child")
    private EntityChild child;

    @OneToMany(mappedBy = "parent1")
    private Set<EntityChild> children1;

    @OneToMany(mappedBy = "parent2")
    private List<EntityChild> children2;

    @Column(name = "only_in_entity")
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
