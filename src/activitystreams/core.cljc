(ns activitystreams.core
  "EDN constructors for ActivityStreams 2.0.")

(def context-key (keyword "@context"))
(def as-context "https://www.w3.org/ns/activitystreams")

(defn ensure-vector [x]
  (cond
    (nil? x) []
    (vector? x) x
    (sequential? x) (vec x)
    :else [x]))

(defn object
  ([type props]
   (merge {context-key as-context :type (name type)} props))
  ([id type props]
   (assoc (object type props) :id id)))

(defn actor [id type props]
  (object id type props))

(defn activity
  [type {:keys [id actor object target result to cc published] :as props}]
  (cond-> (merge {context-key as-context :type (name type)}
                 (dissoc props :id :actor :object :target :result :to :cc :published))
    id (assoc :id id)
    actor (assoc :actor actor)
    object (assoc :object object)
    target (assoc :target target)
    result (assoc :result result)
    to (assoc :to (ensure-vector to))
    cc (assoc :cc (ensure-vector cc))
    published (assoc :published published)))

(defn note [props] (object :Note props))
(defn person [props] (object :Person props))
(defn create [props] (activity :Create props))
(defn update-activity [props] (activity :Update props))
(defn delete [props] (activity :Delete props))
(defn follow [props] (activity :Follow props))
(defn like [props] (activity :Like props))
(defn announce [props] (activity :Announce props))

(defn activity? [x]
  (and (map? x)
       (contains? #{"Create" "Update" "Delete" "Follow" "Like" "Announce" "Accept" "Reject"}
                  (:type x))))

(defn errors [x]
  (cond-> []
    (not (map? x)) (conj {:error :activitystreams/document-must-be-map})
    (and (map? x) (nil? (:type x))) (conj {:error :activitystreams/missing-type})))

(defn validate [x]
  (let [es (errors x)]
    {:valid? (empty? es) :errors es}))
