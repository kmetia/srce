(ns kmet.srce.test-guards
  "Guard: the kernel is self-contained and the retired store stays gone.

   Its namespaces may require other kmet.srce.* namespaces and third-party
   code, but nothing else under kmet.* — no kmet application code and no other
   kmet library. `kmet.libs.scope` was deleted at landing 4e, so the name stays
   a tripwire rather than a dependency."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private kernel-sources
  (->> (fs/glob "src" "**/*.{clj,cljc}")
       (map str)
       sort))

(defn- required-namespaces
  "The names a source file requires, read from its ns form."
  [file]
  (let [text (slurp file)
        ns-form (or (some-> (re-find #"\(ns\s[^\n]+\n([\s\S]*?)\n\n" text) second)
                    (some-> (re-find #"\(ns\s[^\n]+\n([\s\S]*)$" text) second)
                    "")]
    (->> (re-seq #"\[([a-z][a-z0-9.\-]*)" ns-form)
         (map (comp symbol second)))))

(deftest kernel-sources-are-real
  (is (seq kernel-sources) "the guard found the kernel sources")
  (is (every? #(str/ends-with? % ".clj") kernel-sources)))

(deftest kernel-requires-only-its-own-namespaces
  (testing "no foreign kmet namespace"
    (doseq [f kernel-sources
            :let [kmet-deps (filter #(str/starts-with? (str %) "kmet.")
                                    (required-namespaces f))
                  foreign (remove #(str/starts-with? (str %) "kmet.srce")
                                  kmet-deps)]
            :when (seq foreign)]
      (is false (str f " requires foreign kmet code: " (vec foreign))))))

(deftest kernel-does-not-require-the-retired-store
  (doseq [f kernel-sources]
    (is (not (str/includes? (slurp f) "libs.scope"))
        (str f " must not name the retired store"))))
