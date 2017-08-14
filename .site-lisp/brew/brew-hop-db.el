;; This file is managed by brew.el
;; Don't edit unless you know what you are doing

(setq brew-hop-db
  (list (make-brew-hop
         :name "Brewer's Gold"
         :type (list "Bittering")
         :grown (list "UK" "US")
         :profile "Poor aroma; sharp bittering hop"
         :usage "Bittering for ales"
         :aa (list 8 9)
         :substitute (list "Bullion" "Northern Brewer" "Galena"))
        (make-brew-hop
         :name "Bullion"
         :type (list "Bittering")
         :grown (list "US")
         :profile "Poor aroma; sharp bittering hop"
         :usage "Bittering hope for British style ales"
         :aa (list 8 11)
         :substitute (list "Brewer's Gold" "Northern Brewer"))
        (make-brew-hop
         :name "Centennial"
         :type (list "Bittering")
         :grown (list "US")
         :profile "Spicy, floral, citrus aroma, often referred to as Super Cascade because of the similarity. A clean bittering hop."
         :usage "General purpose bittering, aroma, some dry hopping"
         :aa (list 9 11.5)
         :substitute (list "Cascade" "Columbus"))
        ))

(provide 'brew-hop-db)
