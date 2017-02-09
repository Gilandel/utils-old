ecrire mapper + include converter or supplier/consumer

gestion des list/list/DTO

gestion simple
mapper.map(object, class)
mapper.map(object, class, mode)
mapper.map(object, class, mode, deep)
gestion des converters par champ (function)
gestion des transformers par champ (function)
gestion multi niveau / multi type (DTO, IDO)
gestion du mapping par champ de nom différent
gestion de la mise en cache du mapping (reflection java) via scan
gestion de l'annotation et des types transient

dans le cas de collection non trie, exploiter le stream parallelise

nouvelles proprietes du mappable (@MappableProperty(name, constructor, comparator, mode, deep, converter, targetClass)
possibilite de typer les collections (@MappableProperty(constructor=TreeSet.class, comparator=Comparator) private Set<DTO> test;

support transverse Hibernate via Consumer ou  Function
support avec et sans spring

mettre a dispo des converters de date/String en utilisant les standards J8