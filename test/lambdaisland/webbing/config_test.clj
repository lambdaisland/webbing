(ns lambdaisland.webbing.config-test
  (:require [lambdaisland.webbing.config :as config]
            [clojure.test :refer :all]))

(deftest key->env-var-test
  (is (= (config/key->env-var :foo) "FOO"))
  (is (= (config/key->env-var :foo/bar) "FOO__BAR"))
  (is (= (config/key->env-var "sTrInG") "sTrInG"))
  (is (= (config/key->env-var 'foo) "FOO"))
  (is (= (config/key->env-var 'foo/bar) "FOO__BAR"))
  (is (= (config/key->env-var 'foo*) "FOO_STAR_")))

(deftest coerce-stringly-test
  (is (= (config/coerce-stringly [[:foo {:doc "this one stays a string"} string?]] :foo "123")
         "123"))
  (is (= (config/coerce-stringly [[:foo int?]] :foo "123")
         123))
  (is (= (config/coerce-stringly [[:foo uuid?]] :foo "86550586-4c98-42a5-ba71-a0ac3010db19")
         #uuid "86550586-4c98-42a5-ba71-a0ac3010db19"))
  (is (= (config/coerce-stringly [[:foo uri?]] :foo "https://foo.bar")
         (java.net.URI. "https://foo.bar")))
  (is (= (config/coerce-stringly [[:foo keyword?]] :foo "foo/bar")
         :foo/bar))
  (is (= (config/coerce-stringly [[:foo keyword?]] :foo ":foo/bar")
         :foo/bar))
  (is (= (config/coerce-stringly [[:foo [:map]]] :foo "{:xxx 123}")
         {:xxx 123}))
  (is (thrown? Exception (config/coerce-stringly [[:foo [:map]]] :foo "{:xxx 123"))))

(deftest settings-provider-test
  (is (= false
         ((config/settings-provider [{:my/key false}] {})
          :my/key))))
