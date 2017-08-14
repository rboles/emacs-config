;; my projects
(setq my-project-alist
      '(
         ("scalatron" .
         (:prefix "~/code/Scala/Scalatron"
                  :spaces (("scala"  "/src/main/scala"))
                  ))
         ("admin" .
          (:prefix "~/code/serv/dev/admin"
                   :spaces (("test"   "/src/test/scala/com/syncapse/serv")
                            ("webapp" "/src/main/webapp")
                            ("js"     "/src/main/javascript")
                            ("main"   "/src/main/scala/com/syncapse/serv"))
                   ))
         ("porter" .
          (:prefix "~/code/serv/dev/porter"
                   :spaces (("test"   "/src/main/scala/com/syncapse/serv")
                            ("webapp" "/src/main/webapp")
                            ("main"   "/src/main/scala/com/syncapse/serv"))
                   ))
         ("sandow" .
          (:prefix "~/code/sandow/play"
                   :spaces (("*sandow-webapp*" "/src/main/webapp")
                            ("*sandow*"        "/src/main/scala/org/sboles/sandow"))
                   ))
        ))
