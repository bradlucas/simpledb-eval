(ns simpledb-eval.core
  (:require [simpledb.core :as db]
            [clj-time.core :as ctime]
            [clj-time.format :as tform]
            [clj-time.coerce :as coerce]))

;; The following is example code which uses simpledb to store messages which consist of a simple string.
;; To make things more interesting the messages are saved with a timestamp, date and time when they are saved
;; They also will be saved with an index which will be used later to support deleting.


;; Message Schema
;;
;; |--------+-----------+-----------------------------------|
;; | Column | Type      | Description                       |
;; |--------+-----------+-----------------------------------|
;; | id     | Integer   | Primary Key                       |
;; | text   | String    | Message text                      |
;; | ts     | Timestamp | Long value time stamp             |
;; | date   | Date      | Date in mm/dd/yyyy format         |
;; | time   | hh:mma    | Time in hour:minute:am/ pm format |
;; |--------+-----------+-----------------------------------|


;; DB structures
;;
;; :current-message-id     integer
;; :message-ids            vector of integer id values
;; :messages               map containing messages  {:id :text :ts :date :time}



;; Date time support
(def date-format (tform/formatter "MM/dd/yyyy" (ctime/default-time-zone)))
(def time-format (tform/formatter "hh:mma" (ctime/default-time-zone)))

;; Setup of simpledb...
;; When our program starts you need to call db/init to set things up internally. This part of simpledb
;; creates the local sdb.db file if it doesn't exist already and sets a timer to automatically persist
;; the current database contents to that file every five menutes.
;;
;; If you want to start fresh you can delete the sdb.db file manually before starting your program or
;; call db/clear!.
;;
;; If you want to save then contents of your database from your code call db/persist-db



(defn init! []
  "Initialize our database by creating the required structures"
  (db/put! :current-message-id -1)
  (db/put! :message-ids (list))
  (db/put! :messages {}))

(defn- id->message [id]
  "For a given id return the associated message"
  (db/get-in :messages [id]))

(defn- next-id []
  "Return the next id by incrementing the value in :current-message-id"
  (db/update! :current-message-id inc))

(defn- prepare-new [msg]
  "Prepare our message by adding the next-id, timestamp, date and time to the map"
  (let [next-id (next-id)
        ts (ctime/now)]
    (-> msg
        (assoc :id next-id)
        (assoc :ts (coerce/to-long ts))
        (assoc :date (tform/unparse date-format ts))
        (assoc :time (tform/unparse date-format ts)))))

(defn add-message [message]
  "Add a message to our datbase"
  (let [msg (hash-map :text message)
        {:keys [id] :as final} (prepare-new msg)]
    (db/update! :messages assoc id final)    ;; do the insert into the database of our new message
    (db/update! :message-ids conj id)))      ;; save the new records id in our message-ids list

(defn get-messages []
  "Return all our messages"
  (db/get :messages))

(defn delete-message [id]
  "For a given message id remove it from our database"
  (let [{:keys [message-id]} (id->message id)]
    (when message-id
      (let [new-ids (remove #{id} (db/get :message-ids))]   ;; do the removal of our id from our message-ids list
        (db/put! :message-ids new-ids)                      ;; save the new version of message-ids list
        (db/update! :messages dissoc message-id)            ;; remove the message record with the associated id
        ))))
