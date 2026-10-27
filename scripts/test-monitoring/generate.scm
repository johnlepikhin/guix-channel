;;; Write the inputs of scripts/test-monitoring.sh into the directory given
;;; as the argument: the rule libraries with fixed parameters, and the
;;; metrics the textfile collectors produce from the recorded fixtures.

(use-modules (johnlepikhin system services monitoring)
             (johnlepikhin system services monitoring textfile)
             (guix gexp)
             (ice-9 textual-ports))

(define here (dirname (current-filename)))
(define out (cadr (command-line)))

(define (slurp file)
  (call-with-input-file (string-append here "/" file) get-string-all))

(define (spit file content)
  (call-with-output-file (string-append out "/" file)
    (lambda (port) (display content port))))

(define rules-file
  (@@ (johnlepikhin system services monitoring) alert-rule-groups-file))

(spit "rules.yml"
      (plain-file-content
       (rules-file "rules.yml"
                   (append (host-alert-rules
                            #:network-links '(("lan0" . 1000)))
                           (service-alert-rules)
                           (stack-alert-rules)
                           (network-alert-rules
                            #:uplinks '(("jet" "pppoe-out1")
                                        ("starlink" "ether2"))
                            #:router-ports '(("ether2" . 1000)))))))

(spit "shepherd.prom"
      (render-metrics
       (shepherd-metrics (herd-status-started (slurp "herd-status.txt"))
                         '("home-assistant" "zigbee2mqtt" "absent"))))

(spit "smart.prom"
      (render-metrics
       (smartctl-metrics "/dev/nvme0" (slurp "smartctl-nvme.json") 0)))
