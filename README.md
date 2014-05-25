
Kahnrun
=======

*Rapport de projet*

Projet pour le cours de *Systèmes et réseaux* de Marc Pouzet  
Promo Ulm-Info 2013  
Jonathan Laurent  
Élie Michel


Guide rapide
------------

 * `make` pour créer les exécutables
 * `make clean` pour supprimer les fichiers de compilation
 * `master.native` serveur central
 * `example.native` est le programme généré à partir du réseau de Kahn. Lancer une instance sur chaque machine et ajoute le paramètre `--root` à la dernière afin qu'elle lance le premier processus.
 * `testclient.native` test du serveur central. Avec le paramètre `echo_on` (dans `params.ml`) il se comporte comme un client de tchat très basique.
 * `params.ml` constantes hardcodées à modifier à loisir
 * `ray/` Raytracer non parallélisé
 * `newray/` Raytracer utilisant l'interface des réseaux de Kahn. La version actuelle utilise un effet de bord qui ne la rend efficace qu'avec les threads ou l'implémentation séquentielle mais la modification pour le rendre vraiment parallèle est mineure.



Rapport
=======

Afin de le rendre plus agréable à la fois à lire et à rédiger, ce rapport prend la forme d'une liste de notes thématiques. Elles sont classées par ordre chronologique afin de mettre en évidence l'évolution de nos réflexions.



Petit retour sur la fonction `delay`
------------------------------------

J'ai mis du temps à comprendre l'intérêt de la construction `delay`. Elle semble à première vue tout à fait identique à un simple `return (f x)`. Sauf que non. La version avec un simple `return` calcule en fait *avant* la création du processus la valeur `f x` à retourner et le processus en lui-même est alors très simple : il retourne une valeur et puis c'est tout.

La version `delay` crée un processus à partir de `f` et`x` séparément et qui calcule *une fois lancé* la valeur `f x` avant de la retourner. Ce qui est important est que le calcul se fait dans le processus et permet donc de profiter des mécanismes de parallèlisme de l'implémentation.




Pourquoi la version en réseau m'emmbête
---------------------------------------


Le principal problème avec la version en réseau, c'est l'impossibilité de partager de la mémoire. La mémoire que j'ai besoin de partager n'est pas celle des variables. Celle-là je peux toujours la faire passer par les sockets, avec Marshal et tout. Non, ce qui m'embête c'est de partager les fonctions, le cœur du programme.

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





Le retour de Marshal
---------------------

Bon, en fait il semblerait que le module Marshal fonctionne… Le source de mon erreur est simple : j ne l'avais testé que dans le toplevel. Or, le fonctionnement de ce module est asez simple : il transmet les fonctions sous leur forme compilée. Donc forcément, dans le toplevel — qui ne compile pas — ça ne pouvait pas marcher.

Du coup, maintenant que la version précédemment designée est presque terminée, je vais la finir, puis je vais reprendre l'idée initiale, basée sur Marshal.




Petite réorganisation
---------------------

Puisque je dois de toutes façons reprendre pas mal de trucs, j'en profite pour organiser un peu mieux les fichiers et faire un rappel de ce qu'il y a à voir.

Le premier fichier susceptible de vous intéresser est le fichier `params.ml` qui définit les constantes utilisées dans le reste du projet comme par exemple l'adresse et le port du serveur.

Le fichier `kahn.ml` regroupe ensuite l'interface `Kahn` suivie de ses différentes implémentations. C'est un fichier un peu long et « fourre-tout » pour le moment mais c'est plus clair dans les autres fichiers si tout ce qui concerne cette interface est préfixé par `Kahn.`.

Le module `Handcut` en est une copie complètant l'interface avec les fonctions `import` et `export` proposées dans un précédent paragraphe. Il disparaîtra donc sûrement à terme.

Le fichier `utils.ml` est le passage forcé de tout développement en OCaml : il complète les fonctions de la bibliothèque standard avec de petites fonctions usuelles. S'il devenait trop important, on pourraît envisager de le découper en sections thèmatiques mais ce n'est pour le moment pas nécessaire.




Exemple de programme hautement parallèle : un raytracer
-------------------------------------------------------

Il nous fallait un bon exemple d'utilisation des réseaux de Kahn, c'est à dire un algorithme parallèle et intéressant. Le raytracer est un bon exemple d'une part pour son aspect ludique et d'autre part car il est simple à paralléliser : chaque pixel peut être rendu séparément. Afin de séparer les différents problèmes, j'ai préféré commencer par coder le raytracer le plus proprement possible sans penser aux réseaux de Kahn et ne le plier à cette interface que dans un second temps.

Le raytracer est assez simple, il ne gère en fait pas de rebonds de lumières et devrait plutôt être appeler un rasterizer. Il est plus proche d'un rendu en temps réel (dans l'algo, pas dans l'efficacité…) que d'un véritable rendu de raytracer. Mais il est prévu pour être extensible et devrait être aisément complété.

Il prend en entrée des fichiers dans un format inspiré de celui de POV-ray et peut être exporté depuis Blender à l'aide d'un petit script python.



Organisation de la version en réseau
------------------------------------

J'ai mis vraiment du temps à trouver un organisation qui me convienne et ai eu beaucoup de mal à chacune de mes tentatives à anticiper convenablement les différents problèmes auxquels je devrait faire face, perdant donc un temps incroyable en *refactoring*. Le schéma auquel j'ai fini par aboutir est le suivant :

Dans un but originel de souplesse, je ne voulais pas d'un système centralisé. Le serveur central reste cependant indispensable que les nœuds puissent établir une première connection. Il n'y a en fait qu'un seul paquet par nœud lancé qui ne transite par le serveur maître et une fois tous les nœuds identifiés il peut être interrompu. (En pratique non, ça lève une exception, mais il suffirait de la rattraper).

Chaque nœud est en fait une sorte de routeur faisant l'interface entre des processus locaux et les autres nœuds. Il possède donc une table faisant la correspondance entre des identifiants de cannaux de communication (des entiers en pratique, c'est l'identifiant le plus simple qui soit) et des descripteurs de fichiers.

Lorsqu'un paquet arrive — il est envoyé par un processus en se connectant au socket unix local ou un autre nœud se connectant au socket internet — le nœud regarde dans cette *look-up table* s'il sait où router le fichier. Si c'est le cas, il l'envoie tout simplement à sa cible. Sinon, il envoie à tous les nœuds qu'il connaît une demande (paquet `Wait`) pour que ceux qui connaissent l'identifiant de canal demandé le previennent.




Ce rapport aurait pu être étoffé et remis dans une forme plus classique (il servait plûtôt à communiquer entre nous) mais j'ai pas le temps, on est déjà en retard là…

