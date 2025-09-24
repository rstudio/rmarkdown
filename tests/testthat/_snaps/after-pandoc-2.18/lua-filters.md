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
    
    - <a href="#1-a" id="toc-1-a">1 A</a>
      - <a href="#11-b" id="toc-11-b">1.1 B</a>
    - <a href="#2-c" id="toc-2-c">2 C</a>
      - <a href="#21-d" id="toc-21-d">2.1 D</a>
    
    # 1 A
    
    ## 1.1 B
    
    # 2 C
    
    ## 2.1 D
    
    See [A](#1-a)

