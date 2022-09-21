(ns lambdaisland.webbing.config
  "Convert a Webbing 'setup' into a system 'config'

  Component libraries represent a system as a map, with each key-value pair
  representing a component's identifier and the component value.

  This system map is created by starting a system based on a system 'config', a
  map where keys are component identifier, and values are the configuration
  needed to start said component.

  Webbing creates such a 'config' map based on a 'setup' map, which looks like
  this:

  ```
  {:sources {:settings [] :secrets [] :config []}
   :schemas {:settings [] :secrets []}}
  ```

  Sources are where the config, setting, and secrets, are read from. This can be
  from EDN files, environment variables, command line arguments, etc.

  Schemas are used for validation and coercion, and for getting defaults, e.g.
  for parsing environment variables from strings to other types of values.
  "
  (:require [aero.core :as aero]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.cli :as tools-cli]
            [clojure.walk :as walk]
            [lambdaisland.dotenv :as dotenv]
            [muuntaja.core :as muuntaja]
            [malli.core :as malli]))

(defrecord Setting [k])
(defrecord Secret [k])

(defmethod aero/reader 'setting [_ _ k] (->Setting k))
(defmethod aero/reader 'secret [_ _ k] (->Secret k))

(defn key->env-var
  "Take the key used to identify a setting or secret, and turn it into a string
  suitable for use as an environment variable.

  - if the key is already a strig it is left untouched
  - otherwise it is assumed to be an ident (symbol or keyword)
  - identifiers are uppercased and munged, as per [[munge]]
  - dashes become underscores
  - if the ident is qualified (has a namespace), two underscores are used to
    separate name and namespace"
  [k]
  (if (string? k)
    k
    (str (when (qualified-ident? k)
           (str (str/upper-case (munge (namespace k)))
                "__"))
         (str/upper-case (munge (name k))))))

(defn key->cli-arg
  "Take the key used to identify a setting or secret, and turn it into a string
  suitable for use as CLI argument.

  - if the key is already a strig it is left untouched
  - otherwise it is assumed to be an ident (symbol or keyword)
  - if the ident is qualified (has a namespace), a single dash
    separates name and namespace"
  [k]
  (if (string? k)
    k
    (str (when (qualified-ident? k)
           (str (str/lower-case (namespace k))
                "-"))
         (str/lower-case (name k)))))

(comment
  (key->env-var :foo-bar/baz-baq)
  (key->cli-arg :foo-bar/baz-baq)
  ;; => "FOO_BAR__BAZ_BAQ"
  (key->env-var :baz-baq)
  ;; => "BAZ_BAQ"
  )

(defn some-schema
  "Return the malli schema for a given setting/secret key `k`, if it exists in
  `schemas`, which is a vector of `[key ?props schema]`."
  [schemas k]
  (some #(when (= k (first %))
           (malli/schema (last %)))
        schemas))

(defn coerce-stringly* [schema v]
  (let [coerced (condp contains? (malli/type schema)
                  #{'string?} v
                  #{'uuid?} (java.util.UUID/fromString v)
                  #{'uri?} (java.net.URI. v)
                  #{'keyword? 'simple-keyword? 'qualified-keyword?}
                  (if (= \: (first v))
                    (edn/read-string v)
                    (keyword v))

                  (edn/read-string v))]
    coerced))

(defn coerce-stringly
  "Attempt to coerce/parse values stemming from sources where only strings are
  available, notably environment variables and dotenv files.

  If a malli schema for the setting is provided, then we handle a few specific
  cases (string remains strings, UUID and URI get turned into the right type,
  keywords can be provided with or without leading colon). All other cases are
  passed through [[edn/read-string]], and then validated. If the coerced value
  does not match the schema, we throw.

  Is no schema defined for the setting, then we try to parse
  with [[edn/read-string]], and if that fails we return the string value as-is.
  This is a somewhat naive and brute-force approach that is suitable for rapid
  prototyping, but for greater predictability we recommend providing schemas."
  [schemas k v]
  (assert (string? v))
  (if-let [schema (some-schema schemas k)]
    (coerce-stringly* schema v)
    (try
      (edn/read-string v)
      (catch Exception e
        v))))

(comment
  (coerce-stringly [[:http/port {:doc "port for the http server"} int?]] :http/port "9400")
  (coerce-stringly [[:http/port {:doc "port for the http server"} neg-int?]] :http/port "9400"))

(defn env
  "Source for settings/secrets which reads from environment variables."
  []
  (fn [schemas k]
    (some->>
     (System/getenv (key->env-var k))
     (coerce-stringly schemas k))))

(defn dotenv
  "Source for settings/secrets which reads a dotenv file, defaults to `.env`."
  ([]
   (dotenv ".env" nil))
  ([slurpable]
   (dotenv slurpable nil))
  ([slurpable opts]
   (fn [schemas k]
     (when (.exists (io/file slurpable))
       (some->>
        (get
         (dotenv/parse-dotenv (slurp slurpable) opts)
         (key->env-var k))
        (coerce-stringly schemas k))))))

(defn ^:no-doc schemas->cli-opts [schemas]
  (for [[k ?props s] schemas
        :let [[?props s] (if (map? ?props)
                           [?props s]
                           [nil ?props])
              bool? (= 'boolean? (malli/type s))]]

    [nil (str "--"
              (when bool?
                "[no-]")
              (key->cli-arg k)
              (when-not bool?
                (str " " (str/upper-case (name k)))))
     (:doc ?props)
     :id k

     :parse-fn (if bool?
                 identity
                 (partial coerce-stringly* s))]))

(defn cli-args
  ([]
   (cli-args *command-line-args*))
  ([args]
   (let [parsed-args (memoize
                      (fn [schemas]
                        (tools-cli/parse-opts
                         args
                         (into [["-h" "--help" "Print help information"]]
                               (schemas->cli-opts schemas)))))]
     (fn [schemas k]
       (let [{:keys [options summary errors]} (parsed-args schemas)]
         (when (seq errors)
           (run! println errors)
           (System/exit 1))
         (when (:help options)
           (println summary)
           (System/exit 0))
         (get options k))))))

(defn default-value []
  (fn [schemas k]
    (when-let [?props (some #(when (= k (first %))
                               (second %))
                            schemas) ]
      (when (map? ?props)
        (get ?props :default)))))

(defn settings-source
  "Coerce to 'settings-source', which is a function which takes a key and returns
  either a value or `nil`, in which case the next source is tried.

  Input can be a map (simply key lookup), a function, or anything that Aero can
  read, which means anything that implements [[io/IOFactory]]:
  string (filename), `File`, `Reader`, etc.

  A function source will be passed two args, the schemas for settings/secrets,
  and the key. The function is responsible for coercing based on the schema if
  that's relevant (mostly sources that are string-based and so can't represent
  other types of values). They don't need to actually validate the schemas, this
  will be done afterwards on the returned value."
  [s {:keys [schemas aero-opts]}]
  (when s
    (cond
      (map? s)                    (fn [k] (get s k))
      (fn? s)                     (fn [k] (s schemas k))
      (instance? java.io.File s)  (when (.exists s)
                                    (aero/read-config s aero-opts))
      (satisfies? io/IOFactory s) (aero/read-config s aero-opts)
      :else (throw (ex-info (str "A source for settings/secrets must be a map, function, or clojure.java.io/IOFactory, got "
                                 (type s))
                            {:type ::invalid-setting-source
                             :source s})))))

(defn settings-provider
  "Build up a combined settings provider from multiple sources.

  Sources will be coerced as per [[settings-source]], to find a setting/secret
  sources will be consulted in sequences, returning the first non-nil value
  found. If a schema is provided for the given key it will be validated, and if
  iinvalid, an [[ex-info]] with `{:type ::invalid-setting}` is thrown. If none
  of the sources yield a value then an `ex-info` with
  `{:type ::missing-setting}` is thrown."
  [sources {:keys [schemas] :as opts}]
  (let [sources (keep #(settings-source % opts) sources)]
    (fn [k]
      (let [value (some #(let [v (% k)]
                           (when (some? v)
                             v))
                        sources)]
        (when-let [schema (some-schema schemas k)]
          (when-not (malli/validate schema value)
            (throw (ex-info (str "Invalid value for setting " k ": " (pr-str value)
                                 ", expected " (malli/form schema))
                            {:type ::invalid-setting
                             :key k
                             :value value
                             :schema schema}))))
        (when (nil? value)
          (throw (ex-info (str "No value found for setting/secret " k " in the provided sources.")
                          {:type ::missing-setting
                           :key k})))
        value))))

(defn read-config [sources aero-opts]
  (if (sequential? sources)
    (reduce
     (fn [acc c]
       (merge
        acc
        (if (map? c)
          c
          (aero/read-config c aero-opts))))
     {}
     sources)
    (recur [sources] aero-opts)))

(defn read-setup [{:as   setup
                   :keys [sources schemas]}
                  aero-opts]
  (let [{:keys [settings secrets config]} sources
        setting-provider (settings-provider settings {:aero-opts aero-opts
                                                      :schemas (:settings schemas)})
        secret-provider (settings-provider secrets {:aero-opts aero-opts
                                                    :schemas (:secrets schemas)})]
    (walk/postwalk
     (fn [o]
       (cond
         (instance? Setting o)
         (setting-provider (:k o))

         (instance? Secret o)
         (secret-provider (:k o))

         :else o))
     (read-config config aero-opts))))
