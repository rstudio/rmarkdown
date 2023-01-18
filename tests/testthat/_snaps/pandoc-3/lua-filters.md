# number_sections Lua filter works

    # 1 A
    
    ## 1.1 B
    
    # 2 C
    
    ## 2.1 D
    
    See [A](#a)

---

    # 1 A
    
    ## 1.1 B
    
    # 2 C
    
    ## 2.1 D
    
    See [A](#1-a)

---

    Test
    ================
    
    - [1 A](#1-a)
      - [1.1 B](#11-b)
    - [2 C](#2-c)
      - [2.1 D](#21-d)
    
    # 1 A
    
    ## 1.1 B
    
    # 2 C
    
    ## 2.1 D
    
    See [A](#1-a)

