
Kahnrun
=======

*Rapport de projet*

Projet pour le cours de *Systèmes et réseaux* de Marc Pouzet  
Promo Ulm-Info 2013  
Jonathan Laurent  
Élie Michel


**Note :** Ce `readme` n'a pas pour vocation d'être un guide d'utilisation des sources comme l'est usuellement un `readme`. Il sert à avoir un aperçu des difficultés que l'on a pu rencontrer pendant le projet et peut également nous servir de mémo ou de moyen de communication plus intelligible que des commentaires dans du code.

Afin de le rendre plus agréable à la fois à lire et à rédiger, ce rapport prend la forme d'une liste de notes thématiques. Elles sont classées par ordre chronologique afin de mettre en évidence l'évolution de nos réflexions.



Petit retour sur la fonction `delay`
------------------------------------

J'ai mis du temps à comprendre l'intérêt de la construction `delay`. Elle semble à première vue tout à fait identique à un simple `return (f x)`. Sauf que non. La version avec un simple `return` calcule en fait *avant* la création du processus la valeur `f x` à retourner et le processus en lui-même est alors très simple : il retourne une valeur et puis c'est tout.

La version `delay` crée un processus à partir de `f` et`x` séparément et qui calcule *une fois lancé* la valeur `f x` avant de la retourner. Ce qui est important est que le calcul se fait dans le processus et permet donc de profiter des mécanismes de parallèlisme de l'implémentation.

*Élie, 14.05*





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






Une solution temporaire pour la version en réseau
-------------------------------------------------

Comme dit précédemment, le problème de l'implémentation en réseau est de communiquer aux différentes machines ce qu'elles doivent faire. On ne peut envoyer du code puisqu'il est compilé au moment de l'exécution. Je me suis donc résigné à modifier un peu l'interface proposée afin de la rendre implémentable.

J'avais évoqué l'idée d'enregistrer les différentes fonctions afin d'avoir plusieurs points d'entrée et donc de pouvoir communiquer sur cette base — dire « lance la fonction qui est enregistrée à tel nom » à une machine ». Mais c'est assez lourd pour l'utilisateur de l'interface qui peut alors facilement oublier — ou avoir la flemme — d'enregistrer les fonctions et se retrouver donc avec un réseau exécuté sur une unique machine.

Je me suis alors dirigé vers une autre idée : diviser à la main le réseau entre les différentes machines. Certes, on peut là aussi ne pas prendre la peine de le faire, mais il est au moins plus clair que le programme sera sur une unique machine puisqu'on a désormais « un fichier = une machine ».

Afin de permettre la communication entre ces différents réseaux, il a alors fallu ajouter un nouveau type de canal de communication, nommé *hyperchan*. Ces nouveaux cannaux permettent de communiquer entre machines et doivent donc posséder un identifiant unique. Pour des raisons de lisibilité, j'ai préféré utiliser une chaîne de caractères, ce qui de toutes façons est plus simple à utiliser avec les sockets.

Deux fonctions ont donc été ajoutées : `import` et `export` qui permettent respectivement de rediriger un hyperchan sur un port d'entrée et de rediriger un port de sortie vers un hyperchan. Afin de simplifier l'implémentation, les identifiants d'hyperchan ne doivent pas contenir de point d'exclamation.

Reste à trouver comment implémenter ce mécanisme concrêtement.

J'ai commencé par faire deux versions de l'interface : `Server` et `Client`. La première commençais systématiquement par attendre pendant 5s d'éventuels clients avant de commencer son job habituel de réseau de Kahn. La seconde essayait de se connecter au serveur jusqu'à ce que ça marche avant de lancer son réseau de Kahn.

Mais en fait, mise à part la fonction d'initialisation, il n'y avait en fait aucune différence entre ces deux implémentations. J'ai donc cherché à faire un système peer to peer, d'autant plus que ce serait un système potentiellement plus souple. Chaque nœud — je ne l'appelle plus client puisqu'il n'y a plus de serveur — écoute alors le réseau pour les entrées d'hyperchans d'une part et se connecte au besoin aux autres lorsqu'ils exportent un port.

Le problème est alors de savoir :

 1. Où sont les autres ?
 2. Qui possède l'entrée d'un hyperchan donné ? (On n'a accès qu'à son identifiant a priori.)

Pour cela, il a finalement été nécessaire de conserver un point central au réseau, qui serait connu de tous. Un serveur en gros. Mais ce serveur n'a en fait vraiment pas besoin d'être élaboré et surtout n'est pas un nœud du réseau de calcul.

Ce serveur est donc en fait un serveur de tchat : il écoute et diffuse les messages de chacun à tous les autres. On n'a en effet besoin en pratique que de broadcasting puisqu'il sert uniquement à initialiser les hyperchans, afin de savoir où se situe l'autre machine le partageant. Une fois les deux pairs d'une connexion mutuellement identifiés, le serveur n'est alors plus utilisé.

Ainsi, une fois tous les hyperchans apairés, le serveur n'est plus utile et peut donc être coupé.

La séparation en plusieurs fichiers peut sembler un peu rigide puisqu'elle fixe le nombre de machines. On peut cependant faire tourner plusieurs nœuds sur la même machine donc en prévoire un nombre conséquent dès le début. On peut également par la suite imaginer dupliquer les nœuds et prendre en compte la réponse de la plus réactive. Il faudra cependant être attentif à ce moment à bien gérer les lectures multiples dans un même hyperchan.

*Élie, 15.05*





