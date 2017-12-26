(ns lw-report-db-gen.core
  (:require [clojure.data.csv :as csv]
               [clj-time.core :as t]
               [clj-time.coerce :as c]
               [clj-time.types :as typ]
               [clojure.string :as str]
               [clojure.java.io :as io]
               [clojure.test.check.generators :as gen]
               [clj-time.format :as df]))


   (def NO_OF_USERS 10000)
   (def NO_OF_BRANCHES 2000)
   (def NO_OF_CLIENTS 10000)
   (def CLIENTS_PER_PACKAGE 100)
   (def birthday-from (t/date-time 1970 12 31))
   (def birthday-to (t/date-time 2000 1 1))
   (def d-from (t/date-time 2013 12 31))
   (def d-to (t/date-time 2023 1 1))
   (def loan-term-from 60) 
   (def loan-term-to 720)
   (def status-v ["S001" "S002" "S003" "S004"
                  "S005" "S006" "S007" "S008"])
   (def branches-per-level {0 [1 1] 1 [10 30] 2 [5 20] 3 [10 30] 4 [5 15]})
   (def branch-types ["STD" "POS1" "POS2" "POS3"])
   (def branch-fixed "STD")
   (def fake-date (t/date-time 2017 1 1))

   (def fields-user-branches [:user :branch])
   (def fields-branch-all-desc [:branch-id :child-id :child-type])
   
   (def map-fields {:client [:id :birthday :main-branch-id :fake-date]
                    :client-branch [:client-id :branch-id :fake-date]
                    :loan [:loan-id :disb-date :status :amount
                           :term :user :client-id :fake-date]
                    :lkf [:loan-id :date :days
                          :amount1 :amount2
                          :amount3 :amount4 
                          :amount5 :amount6 
                          :amount7 :amount8]})


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

	(def date-formatter (df/formatter "yyyy-MM-dd"))


   (def root-folder "/home/katerina/")


   (def client-csv-file-name (str root-folder "csv/client.csv"))
   (def loan-csv-file-name (str root-folder "csv/loan.csv"))
   (def lkf-csv-file-name (str root-folder "csv/lkf.csv"))
   (def user-branches-csv-file-name (str root-folder "csv/user-branches.csv"))
   (def branch-all-desc-csv-file-name (str root-folder "csv/branch-all-desc.csv"))
   (def client-branch-csv-file-name (str root-folder "csv/client-branch.csv"))




   (defn rand-birthday [birthday-from birthday-to]
     (let [inc-year (rand-int (- (t/year birthday-to) (t/year birthday-from)))
           inc-month (rand-int (- (t/month birthday-to) (t/month birthday-from)))
           inc-day (rand-int (- (t/day birthday-to) (t/day birthday-from)))]
       (t/plus birthday-from (t/years inc-year)
                             (t/months inc-month)
                             (t/days inc-day))))

   (defn rand-date [d-from d-to]
     (let [inc-year (rand-int (- (t/year d-to) (t/year d-from)))
           inc-month (rand-int (- (t/month d-to) (t/month d-from)))
           inc-day (rand-int (- (t/day d-to) (t/day d-from)))]
       (t/plus d-from (t/years inc-year)
                      (t/months inc-month)
                      (t/days inc-day))))

   (defn rand-branches []
     (inc (rand-int NO_OF_BRANCHES)))

   (defn rand-term []
     (+ loan-term-from
        (rand-int
          (- loan-term-to loan-term-from))))

   (defn rand-status []
     (status-v (rand-int (count status-v))))

   (defn rand-amount []
     (/ (+ 1000M (rand-int (- 1000000000 1000)))
        (int (Math/pow 10 (rand-int 5)))))

   (defn rand-user []
     (inc (rand-int NO_OF_USERS)))

   


   (defn num-branches-for-user []
     (gen/generate
       (gen/frequency [[8 (gen/return 1)]
                       [1.5 (gen/choose 2 (dec NO_OF_BRANCHES))]
                       [0.5 (gen/return NO_OF_BRANCHES)]])))


   (defn gen-branches [user branch]
     {:user user :branch branch})


   (defn id-branches [user]
     (let [number (num-branches-for-user)
           list-branches (into #{} (repeatedly number #(rand-branches)))]
       (mapv #(gen-branches user %) list-branches)))


   (defn gen-users-branches []
     (mapcat  #(id-branches %) (range 1 (inc NO_OF_USERS))))


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




   (defn gen-client [n]
   	 {:id n
   	  :birthday (rand-birthday birthday-from birthday-to)
   	  :main-branch-id (rand-branches)
   	  :fake-date fake-date})


   (defn num-branches-for-client []
     (gen/generate
       (gen/frequency [[9.5 (gen/return 1)]
                       [0.5 (gen/choose 2 5)]])))


   (defn add-branch []
     (let [v (num-branches-for-client)]
       (if (= 1 v)
         []
         (into [] (repeatedly (dec v) #(rand-branches))))))


   (defn client-branch [x]
     (mapv #(hash-map :client-id (:id x)
                      :branch-id % 
                      :fake-date fake-date)
           (cons (:main-branch-id x) (add-branch))))


   (defn num-of-loans []
     (gen/generate
       (gen/frequency [[7 (gen/return 1)]
                       [3 (gen/choose 2 4)]])))
   

   (defn gen-loan [date term client] 
     {:disb-date date 
      :status (rand-status)
      :amount (rand-amount)
      :term term
      :user (rand-user)
      :client-id client
      :fake-date fake-date}) 


   (defn gen-loans [client no-of-loans]
     (let [id (:id client)]
       (repeatedly no-of-loans #(gen-loan (rand-date d-from d-to)
                                             (rand-term)
                                             id))))


   (defn gen-loan-id-client [client first-loan-id no-of-loans]
     (let [loan-client-with-id (gen-loans client no-of-loans)]
       (mapv #(into {} (cons [:loan-id (+ first-loan-id %2)] %1))
             loan-client-with-id
             (range))))


   (defn gen-kf [loan-id date] 
     {:loan-id loan-id :date date :days (rand-int 90)
      :amount1 (rand-amount) :amount2 (rand-amount) 
      :amount3 (rand-amount) :amount4 (rand-amount)
      :amount5 (rand-amount) :amount6 (rand-amount)
      :amount7 (rand-amount) :amount8 (rand-amount)})


   (defn gen-loan-kf [date-loan term loan-id]
 	   (mapv #(gen-kf loan-id
                    (t/plus date-loan (t/days %)))
           (range term)))


   (defn gen-loan-lkf [n]
     (let [date-loan(:disb-date n)
           term-loan (:term n)
           loan-id (:loan-id n)]
       (gen-loan-kf date-loan term-loan loan-id)))


   (defn gen-loans-lkf [loan]
     (into [] (mapcat  #(gen-loan-lkf %) loan)))


   (def current-ids (atom {:client-id 1 :loan-id 1}))


   (defn new-ids [ids no-of-loans]
     {:client-id (inc (:client-id ids))
      :loan-id (+ no-of-loans (:loan-id ids))})


   (defn client-data []
     (let [no-of-loans (num-of-loans)
           {:keys [client-id loan-id]} (swap! current-ids new-ids no-of-loans)
           client-id (dec client-id)
           first-loan-id (- loan-id no-of-loans)
           client (gen-client client-id)
           loan (gen-loan-id-client client first-loan-id no-of-loans)]
       {:client [client]
        :client-branch (client-branch client)
        :loan loan
        :lkf (gen-loans-lkf loan)}))


   (defn clients []
     (repeatedly client-data))



   (defn clojure->csv [x]
     (cond
       (typ/date-time? x) (df/unparse date-formatter x)
       :else (str x))) 

   (defn to-csv-row [fields m]
     (mapv clojure->csv ((apply juxt fields) m)))



   (defn close-writer [writers]
     (doseq [writer writers] 
       (.close (val writer))))



   (defn package->csv [package]
     (into {}
       (for [[k v] (apply merge-with concat package)]
         [k (mapv #(to-csv-row (k map-fields) %) v)])))



   (defn gen-create-table-user-branches []
     (str "CREATE TABLE user_branches \n"                      
          "(\n "
          "user integer NOT NULL, \n "
          "branch integer NOT NULL, \n "
          "CONSTRAINT pk_user_branches PRIMARY KEY(user,branch)\n "
          ")"))


   (defn gen-create-table-branch-all-desc []
     (str "CREATE TABLE branch_all_desc \n"
          "(\n "
          "branch_id integer NOT NULL,\n "
          "child_id integer NOT NULL,\n "
          "child_type varchar(6),\n "
          "CONSTRAINT pk_branch_all_desc PRIMARY KEY (branch_id, child_id)\n "
          ")"))


   (defn gen-create-table-client []
     (str "CREATE TABLE client \n"                      
          "(\n "
          "id integer NOT NULL, \n "
          "birthday date, \n "
          "main_branch_id integer NOT NULL, \n "
          "fake_date date, \n "
          "CONSTRAINT pk_client PRIMARY KEY(id)\n "
          ")"))


   (defn gen-create-table-client-branch []
     (str "CREATE TABLE client_branch \n"                      
          "(\n "
          "client_id integer NOT NULL, \n "
          "branch_id integer NOT NULL, \n "
          "fake_date date, \n "
          "CONSTRAINT pk_client_branch PRIMARY KEY(client_id, branch_id)\n "
          ")"))


   (defn gen-create-table-loan []
     (str "CREATE TABLE loan \n"
          "(\n "
          "loan_id integer NOT NULL,\n "
          "disb_date date,\n "
          "status character(4),\n "
          "amount numeric(18,4),\n "
          "term integer,\n "
          "user integer NOT NULL,\n "
          "client_id integer NOT NULL,\n "
          "fake_date date, \n "
          "CONSTRAINT pk_loan PRIMARY KEY (loan_id)\n "
          ")"))


   (defn gen-create-table-lkf []
     (str "CREATE TABLE lkf \n"
          fields-table
          "PRIMARY KEY(loan_id,date),\n "
          "CONSTRAINT pk_lkf FOREIGN KEY(loan_id) REFERENCES loan(loan_id)\n "
          ")"))


   (defn gen-create-index-table-lkf []
     (str "CREATE INDEX idx_date_lkf ON lkf (date)"))


   (defn gen-insert-table [name path]
     (str "COPY INTO " name
          " FROM '" path
          "' USING DELIMITERS ',','\\n','\"' NULL AS ''"
          ";"
           ))


   (defn save-sql []
     (let [table-client (gen-create-table-client)
     	     table-loan (gen-create-table-loan)
     	     table-lkf (gen-create-table-lkf)
           tables-index-lkf (gen-create-index-table-lkf)
           table-user-branches (gen-create-table-user-branches)
           table-branch-all-desc (gen-create-table-branch-all-desc)
           table-client-branch (gen-create-table-client-branch)
           insert-table-client (gen-insert-table "client" client-csv-file-name)
           insert-table-loan (gen-insert-table "loan" loan-csv-file-name)
           insert-table-lkf (gen-insert-table "lkf" lkf-csv-file-name)
           insert-table-user-branches (gen-insert-table "user_branches"
                                                        user-branches-csv-file-name)
           insert-table-branch-all-desc (gen-insert-table "branch_all_desc"
                                                          branch-all-desc-csv-file-name)
           insert-table-client-branch (gen-insert-table "client_branch"
           	                                             client-branch-csv-file-name)
           text-sql (str table-client
                         "; \n"
                         insert-table-client
                         "; \n"
                         table-loan
                         "; \n"
                         insert-table-loan
                         "; \n"
                         table-lkf
                         "; \n"
                         insert-table-lkf
                         "; \n"
                         tables-index-lkf
                         "; \n"
                         table-user-branches
                         "; \n"
                         insert-table-user-branches
                         "; \n" 
                         table-branch-all-desc
                         "; \n"
                         insert-table-branch-all-desc
                         "; \n"
                         table-client-branch
                         "; \n"
                         insert-table-client-branch
                         "; \n"
                         )]
       (spit (str root-folder "csv/create.sql") text-sql)))



   (defn write-file []
     (let [writer-user-branches (io/writer user-branches-csv-file-name)
           writer-branches-all-desc (io/writer branch-all-desc-csv-file-name)
           writers {:client (io/writer client-csv-file-name)
                    :client-branch (io/writer client-branch-csv-file-name)
                    :loan (io/writer loan-csv-file-name)
                    :lkf (io/writer lkf-csv-file-name)}
           num-iter (/ NO_OF_CLIENTS CLIENTS_PER_PACKAGE)]
       (csv/write-csv writer-user-branches
                      (mapv #(to-csv-row fields-user-branches %)
                            (gen-users-branches)))
       (csv/write-csv writer-branches-all-desc
                      (mapv #(to-csv-row  fields-branch-all-desc %)
                            (mapcat #(gen-parent-child %)
                                    (with-all-descendants (gen-branches-hierarchy)))))

       (doall
         (map (fn [n package]
                (doseq [[k v] (package->csv package)]
                  (csv/write-csv (k writers)
                                 v))
                (println n "of" num-iter "Time:" (java.util.Date.)))
             (range num-iter)
             (partition CLIENTS_PER_PACKAGE (clients))))
       (do
         (.close writer-user-branches)
         (.close writer-branches-all-desc)
         (close-writer writers)
         (save-sql))))
