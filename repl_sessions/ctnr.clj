(ns ctnr)

(require '[clojure.tools.namespace.dir :as ctn-dir]
         '[clojure.tools.namespace.repl :as ctn-repl]
         'lambdaisland.webbing.dev)
(:clojure.tools.namespace.dir/time ctn-repl/refresh-tracker)

(#'ctn-dir/modified-files
 ctn-repl/refresh-tracker
 (:clojure.tools.namespace.dir/files ctn-repl/refresh-tracker))

(ctn-dir/scan-dirs ctn-repl/refresh-tracker)
1661257834243
1661257797914
