(ns activitystreams.core-test
  (:require [activitystreams.core :as as]
            [clojure.test :refer [deftest is]]))

(deftest builds-objects
  (let [n (as/note {:content "hello"})]
    (is (= "Note" (:type n)))
    (is (= as/as-context ((keyword "@context") n)))))

(deftest builds-activities
  (let [a (as/create {:actor "https://example.test/alice"
                      :object (as/note {:content "hello"})
                      :to "https://www.w3.org/ns/activitystreams#Public"})]
    (is (as/activity? a))
    (is (= ["https://www.w3.org/ns/activitystreams#Public"] (:to a)))
    (is (:valid? (as/validate a)))))
