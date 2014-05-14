
Kahnrun
=======

*Rapport de projet*

Projet pour le cours de *Systèmes et réseaux* de Marc Pouzet  
Promo Ulm-Info 2013  
Jonathan Laurent  
Élie Michel


Afin de le rendre plus agréable à la fois à lire et à rédiger, ce rapport prend la forme d'une liste de notes thématiques. Elles sont classées par ordre chronologique afin de mettre en évidence l'évolution de nos réflexions.


Pourquoi la version en réseau m'emmerde
---------------------------------------


Le principal problème avec la version en réseau, c'est l'impossibilité de partager de la mémoire. La mémoire que j'ai besoin de partager n'est pas celle des variables. Celle-là je peux toujours la faire passer par les sockets, avec Marshall et tout. Non, ce qui m'embête c'est de partager les fonctions, le cœur du programme.

Tant qu'on manipule des processus, tout va bien puisque le type processus, c'est moi qui le choisi. Je peux trè bien imaginer que c'est un simple identifiant et qu'on a des tables faisant la correspondance avec une machine, un pid, etc.

Mais là où le drame survient, c'est au moment des `bind`. À ce moment là, le gros du calcul se passe dans la fonction passée en second argument, celle qui à partir de l'objet retourné par le processus précédent génère un nouveau processus. On a alors affaire directement à une fonction et on ne peut donc pas la manipuler comme on veut. On ne peut en particulier pas la faire transiter sur le réseau, surtout dans le cas où il y a des appels réccursifs dans tous els sens, du style `integers` dans l'exemple donné.

L'idéal serait d'avoir accès à un fork entre machines : copier un processus d'une machine à l'autre, mais avec un code compilé c'est pas possible : dès que les machines ont des architectures différentes plus rien n'est possible. Et si on veut pouvoir justement profiter des points forts de machines différentes dans le réseau, on ne peut pas. Et puis même sans ça, copier un processus avec ses pages mémoires et tout et tout, c'est hyper bas niveau et donc un peu dangereux.

Le même type de problème arrive pour le `return` mais à ce moment c'est moins grave puisqu'on ne manipule là que des valeurs et qu'on peut donc passer par le réseau. Encore que non : en ayant l'esprit un peu tordu – ou simplement en voulant mettre l'implémentation à l'épeuve – on peut tenter de retourner des clôtures, ce que rien n'interdit.

La première solution que je vois à ce problème est de *numéroter* les différentes fonctions clef, celles utilisées dans les `bind`, afin de pouvoir les appeler à travers le réseau à partir de leur identifiant.

Cette idée pose essentiellement deux problèmes :

  - Afin de respecter l'interface et de conserver un système décemment utilisable, on ne peut pas demander à l'utilisateur de numéroter lui-même les processus. Cette numérotation doit donc être automatique, mais nécessite alors un *preprocessing* du code utilisateur, ce qui s'avèrerait très fastidieux à implémenter.

    On peut alors envisager de modifier l'interface afin de demander l'*enregistrement* des fonctions clef auprès de l'interface. À voir.

  - Ces fonctions prennent généralement en argument des paramètres. Il faut donc prévoir un moyen simple de les prendre en compte. Et là où j'ai de sérieux doutes sur la réalisabilité pratique du truc, c'est pour les fonctions récursives mélangeant fonctions et processus, à l'instar de `integers`.

Dans les solutions, on peut alors envisager :

  - De faire du *preprocessing* sur le code OCaml. Lourd à mettre en place et pas très satisfaisant : c'est sale.
  - Créer un autre langage et l'interprêter plutôt que de le compiler. Le travail consiste donc simplement à faire une machine virtuelle distribuée sur les différentes machines disponibles. Le problème est qu'il faut du coup réinventer la roue en mettant les bases du langage (opérations arithmétiques, etc) et on ne peut pas profiter dans les applications de bibliothèques de fonctions existantes. Lourd à mettre en place et pas très satisfaisant : c'est sale et ça donne un truc inutilisable.
  - Ou alors on utilise un langage déjà existant, mais comprendre un interpreteur déjà existant, c'est un travail de longue haleine et on n'a clairement pas le temps de se lancer là-dedans.

Bon, donc en fait j'ai pas de solution actuellement, il faut qu'on y réfléchisse.

*Élie, 14.05*





Petit retour sur la fonction `delay`
------------------------------------

J'ai mis du temps à comprendre l'intérêt de la construction `delay`. Elle semble à première vue tout à fait identique à un simple `return (f x)`. Sauf que non. La version avec un simple `return` calcule en fait *avant* la création du processus la valeur `f x` à retourner et le processus en lui-même est alors très simple : il retourne une valeur et puis c'est tout.

La version `delay` crée un processus à partir de `f` et`x` séparément et qui calcule *une fois lancé* la valeur `f x` avant de la retourner. Ce qui est important est que le calcul se fait dans le processus et permet donc de profiter des mécanismes de parallèlisme de l'implémentation.

*Élie, 14.05*



