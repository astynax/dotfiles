{:user {:plugins [[lein-pprint "1.1.2"]
                  [lein-kibit "0.1.2"]
                  [lein-try "0.4.3"]
                  [lein-exec "0.3.5"]
                  [jonase/eastwood "0.2.1"]
                  [cider/cider-nrepl "0.13.0-SNAPSHOT"]
                  ;;[cider/cider-nrepl "0.9.1"]
                  ;;[refactor-nrepl "2.0.0-SNAPSHOT"]
                  [lein-ancient "0.6.10"]
                  ]
        :dependencies [[slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}
 :repl {:dependencies [^:displace [org.clojure/clojure "1.8.0"]]}
 }
