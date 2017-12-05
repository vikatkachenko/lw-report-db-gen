(ns lw-report-db-gen.core
  (:require [clojure.data.csv :as csv]
               [clj-time.core :as t]
               [clj-time.coerce :as c]
               [clj-time.types :as typ]
               [clojure.string :as str]
               [clojure.java.io :as io]
               [clojure.test.check.generators :as gen]))



   (def d-from (t/local-date 2013 3 20))
   (def d-to (t/local-date 2023 3 20))
   (def NO_OF_USERS 10000)
   (def NO_OF_BRANCHES 2000)
   (def fields-branch [:user :branch])
   (def fields-lkf [:loan_id :date :days
                :amount1 :amount2
                :amount3 :amount4 
                :amount5 :amount6 
                :amount7 :amount8])
   (def fields-loan [:loan_id :disb_date 
                   :status :amount :term :user])
   (def loan-term-from 60) 
   (def loan-term-to 720)
   (def no-of-loans 100)
   (def branches-per-level {0 [1 1] 1 [10 30] 2 [5 20] 3 [10 30] 4 [5 15]})
   (def branch-types ["STD" "POS1" "POS2" "POS3"])
   (def branch-fixed "STD")

																		 
  ; нужно создать каталог root-folder и в нем csv
   (def root-folder "/home/katerina/")
   
   
   
   (def fields-table (str "(\n "
                          "loan_id integer NOT NULL,\n "
                          "date date NOT NULL,\n "
                          "days integer,\n "
                          "amount1 numeric(18,4),\n "
                          "amount2 numeric(18,4),\n "
                          "amount3 numeric(18,4),\n "
                          "amount4 numeric(18,4),\n "
                          "amount5 numeric(18,4),\n "
                          "amount6 numeric(18,4),\n "
                          "amount7 numeric(18,4),\n "
                          "amount8 numeric(18,4),\n "))
   (def fields [:branch-id :child-id :child-type])


   (def status-v ["S001" "S002" "S003" "S004"
                  "S005" "S006" "S007" "S008"])
   (def loan-csv-file-name (str root-folder "loan.csv"))
   (def users-csv-file-name (str root-folder "branches-of-users.csv"))
   (def lkf-csv-file-name (str root-folder "csv/lkf.csv"))
   (def branch-csv-file-name (str root-folder "branch.csv"))
   
   (defn rand-amount []
     (/ (+ 1000M (rand-int (- 1000000000 1000)))
        (int (Math/pow 10 (rand-int 5)))))
   
   
   (defn rand-term []
     (+ loan-term-from
        (rand-int
          (- loan-term-to loan-term-from))))
 

   (defn rand-date [d-from d-to]
     (let [inc-year (rand-int (- (t/year d-to) (t/year d-from)))
           inc-month (rand-int (- (t/month d-to) (t/month d-from)))
           inc-day (rand-int (- (t/day d-to) (t/day d-from)))]
       (t/plus d-from (t/years inc-year)
                      (t/months inc-month)
                      (t/days inc-day))))
           

   (defn rand-status []
     (status-v (rand-int (count status-v))))


   (defn rand-user []
     (inc (rand-int (dec NO_OF_USERS))))


   (defn rand-branches []
     (inc (rand-int (dec NO_OF_BRANCHES))))

   (defn num-branches []
     (gen/generate
       (gen/frequency [[8 (gen/return 1)]
                       [1.5 (gen/choose 2 (dec NO_OF_BRANCHES))]
                       [0.5 (gen/return NO_OF_BRANCHES)]])))


   (defn gen-branches [user branch]
     {:user user :branch branch})


   (defn id-branches [user]
     (let [number (num-branches)
           list-branches (into #{} (repeatedly number #(rand-branches)))]
       (mapv #(gen-branches user %) list-branches)))


   (defn gen-users-branchs []
     (mapcat  #(id-branches %) (range 1 (inc NO_OF_USERS))))
   
   (defn gen-kf [loan-id date] 
     {:loan_id loan-id :date date :days (rand-int 90)
      :amount1 (rand-amount) :amount2 (rand-amount) 
      :amount3 (rand-amount) :amount4 (rand-amount)
      :amount5 (rand-amount) :amount6 (rand-amount)
      :amount7 (rand-amount) :amount8 (rand-amount)})

   (defn gen-loan [loan-id date term] 
     {:loan_id loan-id :disb_date date 
      :status (rand-status)
      :amount (rand-amount)
      :term term
      :user (rand-user)})

   (defn gen-loan-id [n]
     (let [disb-date(rand-date d-from d-to)
           term (rand-term)]
       (gen-loan n disb-date term)))


   (defn gen-loans []
     (mapv #(gen-loan-id %) (range 1 (inc no-of-loans))))




   (defn gen-loan-kf [date-loan term loan-id]
 	 (mapv #(gen-kf loan-id
                    (t/plus date-loan (t/days %)))
           (range term)))

   (defn gen-loan-lkf [n]
     (let [disb-date(rand-date d-from d-to)
           term (rand-term)]
       (gen-loan-kf disb-date term n)))

   (defn gen-loans-lkf []
     (mapcat  #(gen-loan-lkf %) (range 1 (inc no-of-loans))))



   (defn rand-num [[from to]]
     (+ from (rand-int (- to from))))

   (defn num-children [l]
     (if (branches-per-level l)
       (rand-num (branches-per-level l))
       (rand-num (branches-per-level (apply max (keys branches-per-level))))))
 
   (defn rand-typ [] (rand-nth  branch-types))

   (defn item [parent level]
     (if (< level 2)
       {:parent parent :branch-type branch-fixed}
       {:parent parent :branch-type (rand-typ)}))

   (defn children-level [l parent]
     (into {} (map #(vector % (num-children l)) parent)))

   (defn list-item [level child parent]
     (concat 
   	   (map (fn [x y] (repeatedly x #(item y level)))
  		      (vals child)
  		      parent)))

   (defn map-parent [x] (map #(:id %) x))

   (defn amt [x]
     (reduce + (vals x)))

   (defn add-id [structure amt-child id]
     (map #(assoc %1 :id %2)
  	      (flatten structure)
  	      (take (amt amt-child) (range id (inc NO_OF_BRANCHES)))))

   (defn gen-hierarchy-level [l m]
     (apply concat  	  
       (loop [level l id m parent [0] xs []]
         (if (> id NO_OF_BRANCHES)
  	       xs
  	       (let [parent-children (children-level level parent)
  		         branches-of-level (list-item level parent-children parent)
  	             with-id (add-id branches-of-level parent-children id)]
  	         (recur (inc level)
  	                (+ id (amt parent-children))
  	                (map-parent with-id ) 
  	                (conj xs with-id)))))))



   (defn all-descendants
     [branches branch-id]
     (into [] 
       (apply concat 
         (for [{:keys [id parent branch-type]} branches
               :when (== parent branch-id)]
           (conj (all-descendants branches id) (vector id branch-type))))))



   (defn with-all-descendants
     [branches]
     (mapv #(assoc % :children (all-descendants branches (:id %)))
           branches))


   (defn gen-parent-child [x]
     (let [branch-id (:id x)
           map-children (:children x)]
       (map (fn [[desc type]]
              {:branch-id branch-id
               :child-id desc
               :child-type type})
            map-children)))


   (defn gen-branches-hierarchy []
     (gen-hierarchy-level 0 1))



   (defn clojure->csv [x]
     (cond
       (typ/local-date? x) (str (t/year x) "-" (t/month x) "-" (t/day x))
       :else (str x))) 

   (defn to-csv-row [fields m]
        (mapv clojure->csv ((apply juxt fields) m)))




   

   (defn gen-create-table-lkf []
        (str "CREATE TABLE lkf \n"
             fields-table
             "PRIMARY KEY(loan_id,date),\n "
             "CONSTRAINT pk_lkf FOREIGN KEY(loan_id) REFERENCES loan(loan_id)\n "
             ")"))

   (defn gen-create-index-table-lkf []
     (str "CREATE INDEX idx_date_lkf ON lkf (date)"))

   (defn gen-create-table-loan []
     (str "CREATE TABLE loan \n"
          "(\n "
          "loan_id integer NOT NULL,\n "
          "disb_date date,\n "
          "status character(4),\n "
          "amount numeric(18,4),\n "
          "term integer,\n "
          "user integer NOT NULL,\n "
          "CONSTRAINT pk_loan PRIMARY KEY (loan_id)\n "
          ")"))

    (defn gen-create-table-users []
      (str "CREATE TABLE user_branches \n"                      
           "(\n "
           "user integer NOT NULL, \n "
           "branch integer NOT NULL, \n "
           "CONSTRAINT pk_user_branches PRIMARY KEY(user,branch)\n "
           ")"))


    (defn create-table-branch_all_desc []
      (str "CREATE TABLE branch_all_desc \n"
           "(\n "
           "branch-id integer NOT NULL,\n "
           "child-id integer NOT NULL,\n "
           "child-type varchar(6),\n "
           "CONSTRAINT pk_branch_all_desc PRIMARY KEY (branch-id, child-id)\n "
           ")"))

    

   (defn gen-insert-table [name path]
     (str "COPY INTO " name
          " FROM '" path
          "' USING DELIMITERS ',','\\n','\"' NULL AS ''"
          ";"
           ))


   (defn save-sql []
     (let [table-lkf (gen-create-table-lkf)
           tables-index (gen-create-index-table-lkf)
           insert-table-lkf (gen-insert-table "lkf" lkf-csv-file-name)
           table-loan (gen-create-table-loan)
           table-users (gen-create-table-users)
           insert-table-loan (gen-insert-table "loan" loan-csv-file-name)
           insert-table-users (gen-insert-table "user_branches"
                                                users-csv-file-name)
           table-branch (create-table-branch_all_desc)
           insert-table-branch (gen-insert-table "branch_all_desc" branch-csv-file-name)
           text-sql (str table-lkf
                         "; \n"
                         insert-table-lkf
                         "; \n"
                         table-loan
                         "; \n"
                         insert-table-loan
                         "; \n"
                         table-users
                         "; \n"
                         insert-table-users
                         "; \n" 
                         tables-index
                         "; \n"
                         table-branch
                         "; \n"
                         insert-table-branch
                         "; \n"
                         )]
       (spit (str root-folder "create.sql") text-sql)))  



   
   (defn write-file []
     (let [writer-lkf (io/writer lkf-csv-file-name)
           writer-loan (io/writer loan-csv-file-name)
           writer-users (io/writer users-csv-file-name)
           writer-branches (io/writer branch-csv-file-name)]
       (csv/write-csv writer-lkf
                      (mapv #(to-csv-row fields-lkf %)
                            (gen-loans-lkf)))
       (csv/write-csv writer-loan
                      (mapv #(to-csv-row fields-loan %)
                            (gen-loans)))        
       (csv/write-csv writer-users
                 (mapv #(to-csv-row fields-branch %)
                       (gen-users-branchs)))
       (csv/write-csv writer-branches
                      (mapv #(to-csv-row fields  %)
                            (mapcat #(gen-parent-child %)
                                  (with-all-descendants (gen-branches-hierarchy)))))
       (do
         (.close writer-lkf)
         (.close writer-loan)
         (.close writer-users)
         (.close writer-branches)
         (save-sql))))

