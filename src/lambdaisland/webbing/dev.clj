(ns lambdaisland.webbing.dev
  "Dev system, integrated with tools.namespace for full-system reloading.

  A dev system is typically started from helper functions in `user.clj`, you
  register your system setup with [[register!]], then boot the system
  with [[start!]] (aliased to [[go]] because ever since Chestnut that has gotten
  lodged deeply into people's muscle memory).

  [[reset]] will stop the system, reload any 'dirty' namespaces, then restart
  the system. This prevents any old state from cluttering your REPL process.

  [[reset-all]] is similar but will always reload all namespaces.

  Note that this is already a fairly drastic operation, when making good use of
  a REPL you should only occasionally need to call `reset`/`reset-all`."
  (:require [k16.gx.beta.system :as gx-sys]
            [lambdaisland.webbing.util :as util]
            [lambdaisland.webbing.config :as config]
            [clojure.tools.namespace.repl :as repl]
            [lambdaisland.glogc :as log]))

(create-ns 'webbing.restart-fns)
(repl/disable-reload! (find-ns 'k16.gx.beta.system))
(repl/disable-reload! (find-ns 'webbing.restart-fns))

;; tools.namespace.repl uses a timestamp to determine which namespace have
;; changed since the last `reset`. This timestamp normally only gets set the
;; first time `reset` is called, meaning the first reset reloads everything.
;; Instead we only want it to reload namespaces that changed since the system
;; booted, so we set the namespace here upon loading.
(alter-var-root #'repl/refresh-tracker assoc
                :clojure.tools.namespace.dir/time
                (System/currentTimeMillis))

(def ^:dynamic *sys-id*
  "Default system id that the dev system gets registered under in the gx registry.
  Can also be passed in explicitly, which is useful if you have multiple systems
  within the same process."
  ::system)

(defn system
  ([]
   (system *sys-id*))
  ([sys-id]
   (get @gx-sys/registry* sys-id)))

(defn register!
  "Register the 'setup' for the dev system.

  See Webbing docs for the meaning of system. This will load and register the gx
  system, but not yet start it."
  ([setup]
   (register! *sys-id* setup))
  ([sys-id setup-sym]
   (let [setup (doto ((requiring-resolve setup-sym)) prn)
         graph (config/read-setup setup {:profile :dev})]
     (gx-sys/register! sys-id {:graph graph
                               :lambdaisland.webbing/setup setup
                               :lambdaisland.webbing/setup-sym setup-sym}))))

(defn unregister!
  "Remove the system from the gx registry again."
  ([]
   (unregister! *sys-id*))
  ([sys-id]
   (swap! gx-sys/registry* dissoc sys-id)))

(defn signal!
  "Send a signal to the gx system.

  Unwraps `java.util.concurrent.ExecutionException`, so you get an [[ex-info]]
  instead."
  [sys-id k & args]
  (assert (get-in @gx-sys/registry* [sys-id :lambdaisland.webbing/setup])
          (str "No setup found for " sys-id))
  @(util/unwrap-concurrent-ex
    (apply gx-sys/signal! sys-id k args)))

(defn start!
  "Start the dev system.

  Optionally takes a system id, and collection of keys to start. When omitted
  starts all keys."
  ([]
   (start! nil))
  ([{:keys [keys sys-id]
     :or {sys-id *sys-id*}}]
   (let [keys (or keys (get-in @gx-sys/registry* [sys-id :lambdaisland.webbing/setup :keys]))]
     (log/info :dev-system/starting {:keys (if (seq keys) keys :all) :sys-id sys-id})
     (signal! sys-id :gx/start keys)
     :started)))

(defn stop!
  "Stop the dev system."
  ([]
   (stop! nil))
  ([{:keys [sys-id]
     :or {sys-id *sys-id*}}]
   (signal! sys-id :gx/stop)
   :stopped))

;; respect the legacy
(def go
  "Convenience alias for [[start!]]."
  start!)

;; gx currently does not fall back to calling start/stop when suspend/resume
;; isn't implemented for a component, so these are not really useful right now.
;; Instead we'll call refresh based on a full start/stop cycle.

;; (defn suspend []
;;   (signal! :gx/suspend)
;;   :suspended)

;; (defn resume []
;;   (signal! :gx/resume)
;;   :resumed)

(defn restart
  "Re-register the system from the previously used setup, and start it.

  This is the callback that tools.namespace calls at the end of
  a [[reset]]/[[reset-all]]. Not primarily intended for public use, but could
  have other uses."
  ([]
   (restart *sys-id*))
  ([sys-id]
   (prn "restarting" sys-id)
   (register! sys-id (get-in @gx-sys/registry* [sys-id :lambdaisland.webbing/setup-sym]))
   (go {:sys-id sys-id})))

(defn bound-restart-sym []
  (let [restart-sym (gensym "webbing.restart-fns/restart")
        sys-id *sys-id*]
    (intern (symbol (namespace restart-sym)) (symbol (name restart-sym))
            (fn []
              (with-bindings {(resolve `*sys-id*) sys-id}
                ((resolve `restart)))))
    restart-sym))

(defn reset
  "Stop the system, reload all changed namespaces, and restart."
  ([]
   (stop!)
   (repl/refresh :after (bound-restart-sym)))
  ([sys-id]
   (binding [*sys-id* sys-id]
     (reset))))

(defn reset-all
  "Stop the system, reload all namespaces, and restart.

  Note that this might load more namespaces than were originally loaded, which
  is a common source of reload issues.
  See [[clojure.tools.namespace.repl/set-fresh-dirs]] to limit the directories
  that get scanned for namespaces to (re)load."
  ([]
   (stop!)
   (repl/refresh-all :after (bound-restart-sym)))
  ([sys-id]
   (binding [*sys-id* sys-id]
     (reset-all))))
