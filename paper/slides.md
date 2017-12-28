--- 

# Clustering by fast search and find of density peaks
## Alex Rodriguez and Alessandro Laio
### Science - Vol 344 - Issue 6191

--- 

# Clustering

Los algoritmos de clustering buscan clasificar datos en categorías basándose en
las similitudes entre los mismos.

---

# Assumptions

Cada tipo de algoritmo asume que nuestros datos tienen ciertas caracteristicas:

+ k-means asume que nuestros datos son compactos y que cada cluster tiene
  aproximadamente la misma cantidad de elementos (¿datos no esféricos?).
+ HClust asume ... (¿?).
+ Density-based clustering en general asumen que los datos se encuentran
  agrupados en nubes de cierta densidad, y que bajo cierto umbral los datos son
  ruido (¿qué umbral?).

---

# Density peaks based clustering

¿Qué asume el método basado en **density-peaks**?

+ El centro de cada cluster está rodedo por vecinos de menor densidad.
+ Cada uno de estos está relativamente lejos de otro punto de alta densidad.

--- 

# How?

Sobre cada punto se definen dos métricas:

+ \rho: dist


