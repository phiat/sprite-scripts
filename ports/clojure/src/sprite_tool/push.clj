(ns sprite-tool.push
  "sprite-tool push: Push local file or directory to a sprite."
  (:require [sprite-tool.sprite :as sprite])
  (:import [java.io File]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]))

(defn- usage []
  (println "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]")
  (println)
  (println "Examples:")
  (println "  sprite-tool push ./file.txt /home/sprite/file.txt")
  (println "  sprite-tool push ./mydir /home/sprite/mydir")
  (println "  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk")
  (System/exit 1))

(defn run
  "Execute the push subcommand."
  [args]
  (when (< (count args) 2) (usage))

  (let [local-path  (nth args 0)
        remote-path (nth args 1)
        sprite-name (when (> (count args) 2) (nth args 2))

        sprite-args (if sprite-name ["-s" sprite-name] [])
        local-file  (File. ^String local-path)]

    (when-not (.exists local-file)
      (binding [*out* *err*]
        (println (str "Error: " local-path " does not exist")))
      (System/exit 1))

    (if (.isDirectory local-file)
      ;; Push directory
      (do
        (println (str "Pushing directory: " local-path " -> " remote-path))
        (let [parent (.getParent local-file)
              base   (.getName local-file)

              ;; tar czf - -C parent base | sprite exec ... bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
              tar-pb (doto (ProcessBuilder. ^java.util.List
                                            ["tar" "czf" "-" "-C" parent base])
                       (.redirectOutput ProcessBuilder$Redirect/PIPE)
                       (.redirectError ProcessBuilder$Redirect/INHERIT))
              tar-proc (.start tar-pb)

              sprite-cmd (into ["sprite" "exec"] (concat sprite-args
                                                         ["bash" "-c"
                                                          (str "mkdir -p '" remote-path
                                                               "' && tar xzf - -C '" remote-path
                                                               "' --strip-components=1")]))
              sprite-pb (doto (ProcessBuilder. ^java.util.List sprite-cmd)
                          (.redirectOutput ProcessBuilder$Redirect/INHERIT)
                          (.redirectError ProcessBuilder$Redirect/INHERIT))
              sprite-proc (.start sprite-pb)

              ;; Pipe tar stdout -> sprite stdin
              tar-out   (.getInputStream tar-proc)
              sprite-in (.getOutputStream sprite-proc)
              pipe-thread (Thread.
                           (fn []
                             (try
                               (let [buf (byte-array 8192)]
                                 (loop []
                                   (let [n (.read tar-out buf)]
                                     (when (pos? n)
                                       (.write sprite-in buf 0 n)
                                       (recur)))))
                               (catch Exception _)
                               (finally
                                 (.close sprite-in)))))]
          (.start pipe-thread)
          (.join pipe-thread)
          (.waitFor tar-proc)
          (let [exit (.waitFor sprite-proc)]
            (when-not (zero? exit)
              (binding [*out* *err*]
                (println "Error: push failed"))
              (System/exit 1)))))

      ;; Push file
      (do
        (println (str "Pushing file: " local-path " -> " remote-path))
        (let [remote-dir (or (.getParent (File. ^String remote-path)) "/")
              sprite-cmd (into ["sprite" "exec"] (concat sprite-args
                                                         ["bash" "-c"
                                                          (str "mkdir -p '" remote-dir
                                                               "' && cat > '" remote-path "'")]))
              pb (doto (ProcessBuilder. ^java.util.List sprite-cmd)
                   (.redirectInput (File. ^String local-path))
                   (.redirectOutput ProcessBuilder$Redirect/INHERIT)
                   (.redirectError ProcessBuilder$Redirect/INHERIT))
              proc (.start pb)
              exit (.waitFor proc)]
          (when-not (zero? exit)
            (binding [*out* *err*]
              (println "Error: push failed"))
            (System/exit 1)))))

    (println "Done.")))
