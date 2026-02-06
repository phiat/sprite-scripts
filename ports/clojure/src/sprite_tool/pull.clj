(ns sprite-tool.pull
  "sprite-tool pull: Pull file or directory from a sprite."
  (:require [clojure.string :as str]
            [sprite-tool.sprite :as sprite])
  (:import [java.io File]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]))

(defn- usage []
  (println "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]")
  (println)
  (println "Examples:")
  (println "  sprite-tool pull /home/sprite/file.txt ./file.txt")
  (println "  sprite-tool pull /home/sprite/mydir ./mydir")
  (println "  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk")
  (System/exit 1))

(defn run
  "Execute the pull subcommand."
  [args]
  (when (< (count args) 2) (usage))

  (let [remote-path (nth args 0)
        local-path  (nth args 1)
        sprite-name (when (> (count args) 2) (nth args 2))

        sprite-args (if sprite-name ["-s" sprite-name] [])

        ;; Check if remote is directory or file
        check-cmd (into ["sprite" "exec"] (concat sprite-args
                                                   ["bash" "-c"
                                                    (str "[ -d '" remote-path "' ] && echo dir || echo file")]))
        check-result (sprite/run-process check-cmd :inherit-io false :capture true)
        is-dir (= "dir" (str/trim (or (:stdout check-result) "")))]

    (if is-dir
      ;; Pull directory
      (do
        (println (str "Pulling directory: " remote-path " -> " local-path))
        (let [local-dir (File. ^String local-path)]
          (.mkdirs local-dir))

        ;; sprite exec ... tar czf - -C REMOTE . | tar xzf - -C LOCAL
        (let [sprite-cmd (into ["sprite" "exec"] (concat sprite-args
                                                          ["tar" "czf" "-" "-C" remote-path "."]))
              sprite-pb (doto (ProcessBuilder. ^java.util.List sprite-cmd)
                          (.redirectOutput ProcessBuilder$Redirect/PIPE)
                          (.redirectError ProcessBuilder$Redirect/INHERIT))
              sprite-proc (.start sprite-pb)

              tar-pb (doto (ProcessBuilder. ^java.util.List
                                            ["tar" "xzf" "-" "-C" local-path])
                       (.redirectOutput ProcessBuilder$Redirect/INHERIT)
                       (.redirectError ProcessBuilder$Redirect/INHERIT))
              tar-proc (.start tar-pb)

              ;; Pipe sprite stdout -> tar stdin
              sprite-out (.getInputStream sprite-proc)
              tar-in     (.getOutputStream tar-proc)
              pipe-thread (Thread.
                           (fn []
                             (try
                               (let [buf (byte-array 8192)]
                                 (loop []
                                   (let [n (.read sprite-out buf)]
                                     (when (pos? n)
                                       (.write tar-in buf 0 n)
                                       (recur)))))
                               (catch Exception _)
                               (finally
                                 (.close tar-in)))))]
          (.start pipe-thread)
          (.join pipe-thread)
          (.waitFor sprite-proc)
          (let [exit (.waitFor tar-proc)]
            (when-not (zero? exit)
              (binding [*out* *err*]
                (println "Error: pull failed"))
              (System/exit 1)))))

      ;; Pull file
      (do
        (println (str "Pulling file: " remote-path " -> " local-path))
        (let [local-file (File. ^String local-path)
              parent-dir (.getParentFile local-file)]
          (when parent-dir
            (.mkdirs parent-dir)))

        ;; sprite exec ... cat REMOTE > local
        (let [sprite-cmd (into ["sprite" "exec"] (concat sprite-args
                                                          ["cat" remote-path]))
              pb (doto (ProcessBuilder. ^java.util.List sprite-cmd)
                   (.redirectOutput (ProcessBuilder$Redirect/to (File. ^String local-path)))
                   (.redirectError ProcessBuilder$Redirect/INHERIT))
              proc (.start pb)
              exit (.waitFor proc)]
          (when-not (zero? exit)
            (binding [*out* *err*]
              (println "Error: pull failed"))
            (System/exit 1)))))

    (println "Done.")))
