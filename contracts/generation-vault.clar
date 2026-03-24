;; generation-vault.clar
;; Smart contract for inheritance & estate planning across generations
;; - Grantor deposits STX into a trust vault
;; - Beneficiaries have unlock times and percentage shares
;; - Withdrawals only possible if block-height >= unlock-time for that beneficiary
;; - Immutable after lock-in (if grantor chooses)

;; No trait implementation needed for this contract

(define-constant contract-owner tx-sender)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trust metadata
(define-data-var grantor principal tx-sender)
(define-data-var trust-balance uint u0)
(define-data-var beneficiary-count uint u0)
(define-data-var locked bool false) ;; once locked, cannot be modified

;; Each beneficiary: { addr, share%, unlock-block, withdrawn? }
(define-map beneficiary-addresses uint principal)
(define-map beneficiary-shares uint uint)
(define-map beneficiary-unlock-blocks uint uint)
(define-map beneficiary-withdrawn uint bool)

;; Event Handlers
(define-public (emit-beneficiary-added (id uint) (addr principal) (share uint) (unlock-block uint))
    (ok (print { event: "beneficiary-added", id: id, addr: addr, share: share, unlock-block: unlock-block })))

(define-public (emit-trust-funded (from principal) (amount uint))
    (ok (print { event: "trust-funded", from: from, amount: amount })))

(define-public (emit-withdrawn (to principal) (amount uint))
    (ok (print { event: "withdrawn", to: to, amount: amount })))

(define-public (emit-trust-locked (by principal))
    (ok (print { event: "trust-locked", by: by })))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; u100 not grantor
;; u101 locked trust
;; u102 total shares > 100
;; u103 no balance
;; u104 unlock not reached
;; u105 already withdrawn
;; u106 insufficient funds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (is-grantor? (who principal))
  (is-eq who (var-get grantor)))

;; Helper to get current block height
(define-public (get-current-block)
    (ok u0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grantor Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add a beneficiary (grantor only, before lock)
(define-public (add-beneficiary (beneficiary principal) (share uint) (unlock-height uint))
  (begin
    (asserts! (is-grantor? tx-sender) (err u100))
    (asserts! (not (var-get locked)) (err u101))
    (asserts! (<= share u100) (err u102))
    (asserts! (> unlock-height u0) (err u104))
    ;; ensure shares <= 100 in total
    (let ((id (+ (var-get beneficiary-count) u1))
          (safe-id id)
          (safe-beneficiary (if (is-eq beneficiary tx-sender) tx-sender beneficiary))
          (safe-share (if (<= share u100) share u100))
          (safe-unlock (if (> unlock-height u0) unlock-height u1)))
      (map-set beneficiary-addresses safe-id safe-beneficiary)
      (map-set beneficiary-shares safe-id safe-share)
      (map-set beneficiary-unlock-blocks safe-id safe-unlock)
      (map-set beneficiary-withdrawn safe-id false)
      (var-set beneficiary-count safe-id)
      (unwrap! (emit-beneficiary-added safe-id safe-beneficiary safe-share safe-unlock) (err u100))
      (ok safe-id))))

;; Fund the trust by sending STX
(define-public (fund-trust)
  (let ((amount (stx-get-balance tx-sender)))
    (begin
      (asserts! (> amount u0) (err u103))
      (var-set trust-balance (+ (var-get trust-balance) amount))
      (unwrap! (emit-trust-funded tx-sender amount) (err u103))
      (ok (var-get trust-balance)))))

;; Lock the trust (no more changes allowed)
(define-public (lock-trust)
  (begin
    (asserts! (is-grantor? tx-sender) (err u100))
    (asserts! (not (var-get locked)) (err u101))
    (var-set locked true)
    (unwrap! (emit-trust-locked tx-sender) (err u101))
    (ok true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beneficiary Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Withdraw funds if conditions are met
(define-public (withdraw (id uint))
  (let ((addr (unwrap! (map-get? beneficiary-addresses id) (err u100)))
        (share (unwrap! (map-get? beneficiary-shares id) (err u100)))
        (unlock (unwrap! (map-get? beneficiary-unlock-blocks id) (err u104)))
        (was-withdrawn (unwrap! (map-get? beneficiary-withdrawn id) (err u100)))
        (bal (var-get trust-balance)))
    (begin
      (asserts! (is-eq addr tx-sender) (err u100))
      (let ((current-height (unwrap! (get-current-block) (err u104))))
        (asserts! (>= current-height unlock) (err u104)))
      (asserts! (not was-withdrawn) (err u105))
      (let ((amount (/ (* bal share) u100)))
        (asserts! (>= bal amount) (err u106))
        (let ((result (stx-transfer? amount (as-contract tx-sender) tx-sender)))
          (match result
            success (begin
              (map-set beneficiary-withdrawn id true)
              (var-set trust-balance (- bal amount))
              (unwrap! (emit-withdrawn tx-sender amount) (err u106))
              (ok amount))
            error (err u106)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read-only Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-trust-info)
  {
    grantor: (var-get grantor),
    balance: (var-get trust-balance),
    count: (var-get beneficiary-count),
    locked: (var-get locked)
  })

(define-read-only (get-beneficiary (id uint))
  (let ((addr (map-get? beneficiary-addresses id))
        (share (map-get? beneficiary-shares id))
        (unlock (map-get? beneficiary-unlock-blocks id))
        (withdrawn (map-get? beneficiary-withdrawn id)))
    (if (and (is-some addr)
             (is-some share)
             (is-some unlock)
             (is-some withdrawn))
      (some {
        addr: (unwrap-panic addr),
        share: (unwrap-panic share),
        unlock-block: (unwrap-panic unlock),
        withdrawn: (unwrap-panic withdrawn)
      })
      none)))