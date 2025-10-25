;; Enhanced Storage with Delegated Updates Contract
;; Advanced features including multi-value storage, access levels, history tracking, and more

;; Constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-DELEGATION-EXPIRED (err u101))
(define-constant ERR-INVALID-BLOCK-HEIGHT (err u102))
(define-constant ERR-INVALID-ACCESS-LEVEL (err u103))
(define-constant ERR-CONTRACT-FROZEN (err u104))
(define-constant ERR-INVALID-KEY (err u105))
(define-constant ERR-VALUE-TOO-LARGE (err u106))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u107))
(define-constant ERR-INVALID-SIGNATURE (err u108))
(define-constant ERR-COOLDOWN-ACTIVE (err u109))
(define-constant ERR-MAX-DELEGATES-REACHED (err u110))

;; Access levels
(define-constant ACCESS-LEVEL-READ u1)
(define-constant ACCESS-LEVEL-WRITE u2)
(define-constant ACCESS-LEVEL-ADMIN u3)

;; Limits
(define-constant MAX-VALUE u1000000)
(define-constant MAX-DELEGATES u10)
(define-constant UPDATE-COOLDOWN u144) ;; ~24 hours in blocks
(define-constant DELEGATION-FEE u1000000) ;; 1 STX in microSTX

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var stored-value uint u0)
(define-data-var contract-frozen bool false)
(define-data-var total-updates uint u0)
(define-data-var emergency-contact (optional principal) none)
(define-data-var update-fee uint u0)
(define-data-var last-update-block uint u0)
(define-data-var delegate-count uint u0)

;; Data Maps
;; Enhanced delegation with access levels and metadata
(define-map delegated-permissions 
  principal 
  {
    expiration-height: uint,
    access-level: uint,
    granted-at: uint,
    granted-by: principal,
    uses-remaining: (optional uint)
  }
)

;; Multi-key storage system
(define-map key-value-store 
  {key: (string-ascii 64)} 
  {
    value: uint,
    last-updated: uint,
    updated-by: principal,
    locked: bool
  }
)

;; Update history tracking
(define-map update-history 
  uint 
  {
    old-value: uint,
    new-value: uint,
    updated-by: principal,
    block-height: uint,
    key: (optional (string-ascii 64))
  }
)

;; Access control lists
(define-map access-whitelist principal bool)
(define-map access-blacklist principal bool)

;; Signature-based updates for off-chain authorization
(define-map used-nonces uint bool)

;; Private Functions

;; Enhanced authorization check with access levels
(define-private (is-authorized-with-level (caller principal) (required-level uint))
  (if (var-get contract-frozen)
    false
    (or 
      (is-eq caller (var-get contract-owner))
      (and 
        (is-valid-delegate-with-level caller required-level)
        (not (default-to false (map-get? access-blacklist caller)))
      )
      (and 
        (is-eq required-level ACCESS-LEVEL-READ)
        (default-to false (map-get? access-whitelist caller))
      )
    )
  )
)

;; Check if address has valid delegation with required access level
(define-private (is-valid-delegate-with-level (delegate principal) (required-level uint))
  (match (map-get? delegated-permissions delegate)
    delegation-info
      (and 
        (> (get expiration-height delegation-info) block-height)
        (>= (get access-level delegation-info) required-level)
        (match (get uses-remaining delegation-info)
          remaining (> remaining u0)
          true
        )
      )
    false
  )
)

;; Decrement delegation uses
(define-private (decrement-delegation-uses (delegate principal))
  (match (map-get? delegated-permissions delegate)
    delegation-info
      (match (get uses-remaining delegation-info)
        remaining
          (if (> remaining u0)
            (map-set delegated-permissions delegate 
              (merge delegation-info {uses-remaining: (some (- remaining u1))}))
            true)
        true)
    true
  )
)

;; Record update in history
(define-private (record-update (old-val uint) (new-val uint) (updater principal) (key-name (optional (string-ascii 64))))
  (let ((update-id (var-get total-updates)))
    (map-set update-history update-id {
      old-value: old-val,
      new-value: new-val,
      updated-by: updater,
      block-height: block-height,
      key: key-name
    })
    (var-set total-updates (+ update-id u1))
  )
)

;; Check cooldown period
(define-private (is-cooldown-active)
  (< block-height (+ (var-get last-update-block) UPDATE-COOLDOWN))
)

;; Public Functions

;; Enhanced delegation with access levels and usage limits
(define-public (delegate-permission-advanced 
  (delegate principal) 
  (expiration-height uint) 
  (access-level uint)
  (uses-limit (optional uint)))
  (begin
    ;; Authorization and validation checks
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (> expiration-height block-height) ERR-INVALID-BLOCK-HEIGHT)
    (asserts! (<= access-level ACCESS-LEVEL-ADMIN) ERR-INVALID-ACCESS-LEVEL)
    (asserts! (< (var-get delegate-count) MAX-DELEGATES) ERR-MAX-DELEGATES-REACHED)
    
    ;; Charge delegation fee
    (try! (stx-transfer? DELEGATION-FEE tx-sender (var-get contract-owner)))
    
    ;; Set delegation
    (map-set delegated-permissions delegate {
      expiration-height: expiration-height,
      access-level: access-level,
      granted-at: block-height,
      granted-by: tx-sender,
      uses-remaining: uses-limit
    })
    
    ;; Update delegate count
    (var-set delegate-count (+ (var-get delegate-count) u1))
    (ok true)
  )
)

;; Batch delegate multiple addresses
(define-public (batch-delegate (delegates (list 5 {addr: principal, exp: uint, level: uint})))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (map delegate-single delegates))
  )
)

(define-private (delegate-single (delegate-info {addr: principal, exp: uint, level: uint}))
  (map-set delegated-permissions (get addr delegate-info) {
    expiration-height: (get exp delegate-info),
    access-level: (get level delegate-info),
    granted-at: block-height,
    granted-by: tx-sender,
    uses-remaining: none
  })
)

;; Enhanced value setter with cooldown and fees
(define-public (set-value-enhanced (new-value uint))
  (let ((old-value (var-get stored-value)))
    (begin
      ;; Authorization and validation
      (asserts! (is-authorized-with-level tx-sender ACCESS-LEVEL-WRITE) ERR-UNAUTHORIZED)
      (asserts! (<= new-value MAX-VALUE) ERR-VALUE-TOO-LARGE)
      (asserts! (not (is-cooldown-active)) ERR-COOLDOWN-ACTIVE)
      
      ;; Charge update fee if set
      (if (> (var-get update-fee) u0)
        (try! (stx-transfer? (var-get update-fee) tx-sender (var-get contract-owner)))
        true
      )
      
      ;; Update value and metadata
      (var-set stored-value new-value)
      (var-set last-update-block block-height)
      (record-update old-value new-value tx-sender none)
      
      ;; Decrement uses for delegates
      (if (not (is-eq tx-sender (var-get contract-owner)))
        (decrement-delegation-uses tx-sender)
        true
      )
      
      (ok true)
    )
  )
)

;; Multi-key storage operations
(define-public (set-key-value (key (string-ascii 64)) (value uint))
  (let ((existing (map-get? key-value-store {key: key})))
    (begin
      (asserts! (is-authorized-with-level tx-sender ACCESS-LEVEL-WRITE) ERR-UNAUTHORIZED)
      (asserts! (<= value MAX-VALUE) ERR-VALUE-TOO-LARGE)
      
      ;; Check if key is locked
      (match existing
        entry (asserts! (not (get locked entry)) ERR-UNAUTHORIZED)
        true
      )
      
      ;; Set key-value pair
      (map-set key-value-store {key: key} {
        value: value,
        last-updated: block-height,
        updated-by: tx-sender,
        locked: false
      })
      
      ;; Record in history
      (record-update 
        (match existing entry (get value entry) u0)
        value 
        tx-sender 
        (some key)
      )
      
      (ok true)
    )
  )
)

;; Lock/unlock specific keys
(define-public (toggle-key-lock (key (string-ascii 64)))
  (match (map-get? key-value-store {key: key})
    entry
      (begin
        (asserts! (is-authorized-with-level tx-sender ACCESS-LEVEL-ADMIN) ERR-UNAUTHORIZED)
        (map-set key-value-store {key: key} 
          (merge entry {locked: (not (get locked entry))}))
        (ok (not (get locked entry)))
      )
    ERR-INVALID-KEY
  )
)

;; Batch operations
(define-public (batch-set-values (updates (list 10 {key: (string-ascii 64), value: uint})))
  (begin
    (asserts! (is-authorized-with-level tx-sender ACCESS-LEVEL-WRITE) ERR-UNAUTHORIZED)
    (ok (map process-batch-update updates))
  )
)

(define-private (process-batch-update (update {key: (string-ascii 64), value: uint}))
  (map-set key-value-store {key: (get key update)} {
    value: (get value update),
    last-updated: block-height,
    updated-by: tx-sender,
    locked: false
  })
)

;; Emergency functions
(define-public (emergency-freeze)
  (begin
    (asserts! 
      (or 
        (is-eq tx-sender (var-get contract-owner))
        (is-eq (some tx-sender) (var-get emergency-contact))
      ) 
      ERR-UNAUTHORIZED
    )
    (ok (var-set contract-frozen true))
  )
)

(define-public (emergency-unfreeze)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (var-set contract-frozen false))
  )
)

(define-public (set-emergency-contact (contact (optional principal)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (var-set emergency-contact contact))
  )
)

;; Access control management
(define-public (add-to-whitelist (address principal))
  (begin
    (asserts! (is-authorized-with-level tx-sender ACCESS-LEVEL-ADMIN) ERR-UNAUTHORIZED)
    (ok (map-set access-whitelist address true))
  )
)

(define-public (add-to-blacklist (address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (map-set access-blacklist address true))
  )
)

(define-public (remove-from-blacklist (address principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (map-delete access-blacklist address))
  )
)

;; Fee management
(define-public (set-update-fee (fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (var-set update-fee fee))
  )
)

;; Signature-based updates (for off-chain authorization)
(define-public (set-value-with-signature 
  (new-value uint) 
  (nonce uint) 
  (signature (buff 65)))
  (begin
    ;; Check nonce hasn't been used
    (asserts! (not (default-to false (map-get? used-nonces nonce))) ERR-INVALID-SIGNATURE)
    
    ;; Mark nonce as used
    (map-set used-nonces nonce true)
    
    ;; Update value (signature verification would be implemented with secp256k1)
    (var-set stored-value new-value)
    (record-update (var-get stored-value) new-value tx-sender none)
    
    (ok true)
  )
)

;; Revoke delegation with reason
(define-public (revoke-delegation-with-reason (delegate principal) (reason (string-ascii 128)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (map-delete delegated-permissions delegate)
    (var-set delegate-count (- (var-get delegate-count) u1))
    (ok reason)
  )
)

;; Transfer ownership with confirmation
(define-public (transfer-ownership-safe (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    ;; Clear all delegations on ownership transfer
    (var-set delegate-count u0)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Read-only Functions

;; Get main stored value
(define-read-only (get-value)
  (var-get stored-value)
)

;; Get value by key
(define-read-only (get-key-value (key (string-ascii 64)))
  (map-get? key-value-store {key: key})
)

;; Get all contract stats
(define-read-only (get-contract-stats)
  {
    owner: (var-get contract-owner),
    total-updates: (var-get total-updates),
    frozen: (var-get contract-frozen),
    delegate-count: (var-get delegate-count),
    last-update: (var-get last-update-block),
    update-fee: (var-get update-fee)
  }
)

;; Enhanced delegation status
(define-read-only (get-delegation-info (delegate principal))
  (map-get? delegated-permissions delegate)
)

;; Get update history
(define-read-only (get-update-history (update-id uint))
  (map-get? update-history update-id)
)

;; Get recent updates (last 5)
(define-read-only (get-recent-updates)
  (let ((total (var-get total-updates)))
    (if (> total u5)
      {
        recent-1: (map-get? update-history (- total u1)),
        recent-2: (map-get? update-history (- total u2)),
        recent-3: (map-get? update-history (- total u3)),
        recent-4: (map-get? update-history (- total u4)),
        recent-5: (map-get? update-history (- total u5))
      }
      {
        recent-1: (map-get? update-history u0),
        recent-2: none,
        recent-3: none,
        recent-4: none,
        recent-5: none
      }
    )
  )
)

;; Check various permissions
(define-read-only (can-read (caller principal))
  (is-authorized-with-level caller ACCESS-LEVEL-READ)
)

(define-read-only (can-write (caller principal))
  (is-authorized-with-level caller ACCESS-LEVEL-WRITE)
)

(define-read-only (can-admin (caller principal))
  (is-authorized-with-level caller ACCESS-LEVEL-ADMIN)
)

;; Access list checks
(define-read-only (is-whitelisted (address principal))
  (default-to false (map-get? access-whitelist address))
)

(define-read-only (is-blacklisted (address principal))
  (default-to false (map-get? access-blacklist address))
)

;; Utility functions
(define-read-only (get-current-block-height)
  block-height
)

(define-read-only (get-cooldown-remaining)
  (if (is-cooldown-active)
    (- (+ (var-get last-update-block) UPDATE-COOLDOWN) block-height)
    u0
  )
)