# Epsilon
    epsilon
    all()
    +epsilon
    epsilon / {}
    epsilon / {0}
    epsilon - epsilon
    epsilon & epsilon
    epsilon | epsilon
    epsilon | never
    never | epsilon
* !

---

# Never
    never
    cat()
    any()
    +never
    never / {}
    never / {0}
    never - 0
    epsilon - never
    never - epsilon
    never - never
    epsilon & never
    never & epsilon
    never & never
    never | never
* /

---

# Simple
    0
    cat(0)
    all(0)
    any(0)
    0 / {}
    0 / {0}
    0 / {1}
    0 & 0
    0 | 0
    epsilon - 0
    0 - epsilon
    0 & epsilon
    epsilon & 0
    0 | never
    never | 0
* 0!

---

# SimpleIter
    +0
* 0.0.0.

---

# SimpleEps
    0 | epsilon
    epsilon | 0
* .0!

---

# SimpleTerm
    0 - never
    0 & never
    never & 0
* 0/

---

# SameCat
    (0 : a) - 0
* 0a!0!

---

# SimpleCat
    (0 : a) - 1
* 0a!1!
* 1110a!0001!

---

# SimpleAll
    (0 : a) & (1 : b)
    (1 : b) & (0 : b)
* 0a!1b!!
* 1b!0a!!
* 0a!0001b!!
* 1b!1110a!!

---

# SimpleAny
    (0 : a) | (1 : b)
    (1 : b) | (0 : b)
* 0a!.1b!!
* 1b!.0a!!
* 0a!.0001b!!
* 1b!.1110a!!
