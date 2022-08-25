(ns lambdaisland.webbing.prod
  (:require [k16.gx.beta.system :as gx-sys]
            [lambdaisland.webbing.util :as util]
            [lambdaisland.webbing.config :as config]
            [clojure.pprint]))

(def sys-id ::system)

(defn go [setup]
  (let [config  (config/read-setup setup {:profile :prod})
        _       (gx-sys/register! sys-id {:graph config :lambdaisland.webbing/setup setup})
        system  (util/unwrap-concurrent-ex
                 (gx-sys/signal! sys-id :gx/start))
        runtime (java.lang.Runtime/getRuntime)]
    @system
    (.addShutdownHook
     runtime
     (Thread. (fn []
                (util/unwrap-concurrent-ex
                 (gx-sys/signal! sys-id :gx/stop))
                (shutdown-agents))))
    @(promise)))
