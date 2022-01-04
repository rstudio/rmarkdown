# Converting bib file is working

    list(list(author = list(list(family = "Conceição", given = "Sérgio")), 
        "container-title" = "Portuguese History", id = "conc2021", 
        issue = "1", issued = list("date-parts" = list(list(2021L))), 
        title = "História da habitação", type = "article-journal"))

---

     [1] "["                                               
     [2] "  {"                                             
     [3] "    \"author\": ["                               
     [4] "      {"                                         
     [5] "        \"family\": \"Conceição\","              
     [6] "        \"given\": \"Sérgio\""                   
     [7] "      }"                                         
     [8] "    ],"                                          
     [9] "    \"container-title\": \"Portuguese History\","
    [10] "    \"id\": \"conc2021\","                       
    [11] "    \"issue\": \"1\","                           
    [12] "    \"issued\": {"                               
    [13] "      \"date-parts\": ["                         
    [14] "        ["                                       
    [15] "          2021"                                  
    [16] "        ]"                                       
    [17] "      ]"                                         
    [18] "    },"                                          
    [19] "    \"title\": \"História da habitação\","       
    [20] "    \"type\": \"article-journal\""               
    [21] "  }"                                             
    [22] "]"                                               

---

     [1] "---"                                  
     [2] "nocite: \"[@*]\""                     
     [3] "references:"                          
     [4] "- author:"                            
     [5] "  - family: Conceição"                
     [6] "    given: Sérgio"                    
     [7] "  container-title: Portuguese History"
     [8] "  id: conc2021"                       
     [9] "  issue: 1"                           
    [10] "  issued: 2021"                       
    [11] "  title: História da habitação"       
    [12] "  type: article-journal"              
    [13] "---"                                  
    [14] ""                                     

