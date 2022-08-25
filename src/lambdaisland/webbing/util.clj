(ns lambdaisland.webbing.util)

(defmacro unwrap-concurrent-ex
  [& body]
  `(try
     ~@body
     (catch java.util.concurrent.ExecutionException ex#
       (throw (ex-cause ex#)))))
