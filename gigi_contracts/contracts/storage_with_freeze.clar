;; Enhanced Emergency Freeze Storage Contract
;; A comprehensive storage contract with emergency freeze and advanced features

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_STORAGE_FROZEN (err u402))
(define-constant ERR_INVALID_KEY (err u403))
(define-constant ERR_KEY_NOT_FOUND (err u404))
(define-constant ERR_INVALID_ACCESS_LEVEL (err u405))
(define-constant ERR_QUOTA_EXCEEDED (err u406))
(define-constant ERR_INVALID_EXPIRY (err u407))
(define-constant ERR_DATA_EXPIRED (err u408))
(define-constant ERR_BACKUP_FAILED (err u409))

;; Access Levels
(define-constant ACCESS_PUBLIC u0)
(define-constant ACCESS_PRIVATE u1)
(define-constant ACCESS_RESTRICTED u2)

;; Data Variables
(define-data-var storage-frozen bool false)
(define-data-var freeze-reason (string-utf8 256) u"")
(define-data-var freeze-timestamp uint u0)
(define-data-var total-entries uint u0)
(define-data-var max-entries uint u1000)
(define-data-var contract-paused bool false)
(define-data-var backup-enabled bool true)

;; Data Maps
(define-map storage-data 
  { key: (string-ascii 64) } 
  { 
    value: (string-utf8 512), 
    updated-at: uint,
    created-at: uint,
    access-level: uint,
    expires-at: (optional uint),
    tags: (list 5 (string-ascii 32)),
    version: uint
  }
)

;; User permissions and quotas
(define-map user-permissions
  { user: principal }
  { 
    can-write: bool,
    can-delete: bool,
    quota-used: uint,
    quota-limit: uint,
    last-activity: uint
  }
)

;; Access control for restricted data
(define-map data-access
  { key: (string-ascii 64), user: principal }
  { granted: bool, granted-at: uint }
)

;; Data history/versioning
(define-map data-history
  { key: (string-ascii 64), version: uint }
  { 
    value: (string-utf8 512),
    updated-at: uint,
    updated-by: principal
  }
)

;; Backup storage
(define-map backup-data
  { backup-id: uint }
  { 
    timestamp: uint,
    entries-count: uint,
    backup-identifier: uint
  }
)

;; Event logs
(define-map event-logs
  { event-id: uint }
  {
    event-type: (string-ascii 32),
    user: principal,
    timestamp: uint,
    details: (string-utf8 256)
  }
)

(define-data-var next-event-id uint u0)
(define-data-var next-backup-id uint u0)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (log-event (event-type (string-ascii 32)) (details (string-utf8 256)))
  (let ((event-id (var-get next-event-id)))
    (map-set event-logs 
      { event-id: event-id }
      {
        event-type: event-type,
        user: tx-sender,
        timestamp: block-height,
        details: details
      }
    )
    (var-set next-event-id (+ event-id u1))
  )
)

(define-private (is-data-expired (key (string-ascii 64)))
  (match (map-get? storage-data { key: key })
    data-entry (match (get expires-at data-entry)
      expiry-time (>= block-height expiry-time)
      false
    )
    true
  )
)

(define-private (has-write-permission (user principal))
  (match (map-get? user-permissions { user: user })
    perms (get can-write perms)
    (is-contract-owner) ;; Owner has all permissions by default
  )
)

(define-private (check-quota (user principal))
  (match (map-get? user-permissions { user: user })
    perms (< (get quota-used perms) (get quota-limit perms))
    true ;; No quota set means unlimited
  )
)

(define-private (increment-quota (user principal))
  (match (map-get? user-permissions { user: user })
    perms (map-set user-permissions 
      { user: user }
      (merge perms { quota-used: (+ (get quota-used perms) u1) })
    )
    true
  )
)

;; Enhanced Public Functions

;; Store data with advanced features
(define-public (store-data-advanced 
  (key (string-ascii 64)) 
  (value (string-utf8 512))
  (access-level uint)
  (expires-at (optional uint))
  (tags (list 5 (string-ascii 32)))
)
  (begin
    ;; Pre-flight checks
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (not (var-get storage-frozen)) ERR_STORAGE_FROZEN)
    (asserts! (> (len key) u0) ERR_INVALID_KEY)
    (asserts! (<= access-level u2) ERR_INVALID_ACCESS_LEVEL)
    (asserts! (has-write-permission tx-sender) ERR_UNAUTHORIZED)
    (asserts! (check-quota tx-sender) ERR_QUOTA_EXCEEDED)
    (asserts! (< (var-get total-entries) (var-get max-entries)) ERR_QUOTA_EXCEEDED)
    
    ;; Validate expiry time
    (match expires-at
      expiry (asserts! (> expiry block-height) ERR_INVALID_EXPIRY)
      true
    )
    
    ;; Store the data
    (map-set storage-data 
      { key: key }
      { 
        value: value,
        updated-at: block-height,
        created-at: block-height,
        access-level: access-level,
        expires-at: expires-at,
        tags: tags,
        version: u1
      }
    )
    
    ;; Store initial version in history
    (map-set data-history
      { key: key, version: u1 }
      {
        value: value,
        updated-at: block-height,
        updated-by: tx-sender
      }
    )
    
    ;; Update counters and quotas
    (var-set total-entries (+ (var-get total-entries) u1))
    (increment-quota tx-sender)
    (log-event "STORE" u"Data stored successfully")
    
    (ok true)
  )
)

;; Update data with versioning
(define-public (update-data-versioned (key (string-ascii 64)) (new-value (string-utf8 512)))
  (begin
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (not (var-get storage-frozen)) ERR_STORAGE_FROZEN)
    (asserts! (has-write-permission tx-sender) ERR_UNAUTHORIZED)
    
    ;; Get existing data
    (match (map-get? storage-data { key: key })
      data-entry (begin
        (asserts! (not (is-data-expired key)) ERR_DATA_EXPIRED)
        
        (let ((new-version (+ (get version data-entry) u1)))
          ;; Update main data
          (map-set storage-data 
            { key: key }
            (merge data-entry { 
              value: new-value, 
              updated-at: block-height,
              version: new-version
            })
          )
          
          ;; Store version history
          (map-set data-history
            { key: key, version: new-version }
            {
              value: new-value,
              updated-at: block-height,
              updated-by: tx-sender
            }
          )
          
          (log-event "UPDATE" u"Data updated successfully")
          (ok new-version)
        )
      )
      ERR_KEY_NOT_FOUND
    )
  )
)

;; Batch operations
(define-public (store-batch (entries (list 10 { key: (string-ascii 64), value: (string-utf8 512) })))
  (begin
    (asserts! (not (var-get storage-frozen)) ERR_STORAGE_FROZEN)
    (asserts! (has-write-permission tx-sender) ERR_UNAUTHORIZED)
    
    (fold store-single-entry entries (ok u0))
  )
)

(define-private (store-single-entry 
  (entry { key: (string-ascii 64), value: (string-utf8 512) })
  (prev-result (response uint uint))
)
  (match prev-result
    success (match (store-data-advanced 
      (get key entry) 
      (get value entry) 
      ACCESS_PUBLIC 
      none 
      (list)
    )
      store-success (ok (+ success u1))
      store-error (err store-error)
    )
    error (err error)
  )
)

;; User management
(define-public (set-user-permissions 
  (user principal) 
  (can-write bool) 
  (can-delete bool)
  (quota-limit uint)
)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (map-set user-permissions
      { user: user }
      {
        can-write: can-write,
        can-delete: can-delete,
        quota-used: u0,
        quota-limit: quota-limit,
        last-activity: block-height
      }
    )
    
    (log-event "PERMISSION_SET" u"User permissions updated")
    (ok true)
  )
)

;; Grant access to restricted data
(define-public (grant-data-access (key (string-ascii 64)) (user principal))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (map-set data-access
      { key: key, user: user }
      { granted: true, granted-at: block-height }
    )
    
    (ok true)
  )
)

;; Emergency functions with enhanced features
(define-public (emergency-freeze-with-reason (reason (string-utf8 256)))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (var-set storage-frozen true)
    (var-set freeze-reason reason)
    (var-set freeze-timestamp block-height)
    
    (log-event "EMERGENCY_FREEZE" reason)
    (ok true)
  )
)

;; Contract pause (more restrictive than freeze)
(define-public (pause-contract)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (var-set contract-paused true)
    (log-event "CONTRACT_PAUSED" u"Contract operations paused")
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (var-set contract-paused false)
    (log-event "CONTRACT_UNPAUSED" u"Contract operations resumed")
    (ok true)
  )
)

;; Backup functionality
(define-public (create-backup)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    (asserts! (var-get backup-enabled) ERR_BACKUP_FAILED)
    
    (let ((backup-id (var-get next-backup-id)))
      (map-set backup-data
        { backup-id: backup-id }
        {
          timestamp: block-height,
          entries-count: (var-get total-entries),
          backup-identifier: (+ (* block-height u1000) backup-id)
        }
      )
      
      (var-set next-backup-id (+ backup-id u1))
      (log-event "BACKUP_CREATED" u"System backup created")
      (ok backup-id)
    )
  )
)

;; Cleanup expired data
(define-public (cleanup-expired (keys (list 20 (string-ascii 64))))
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (fold cleanup-single-key keys (ok u0))
  )
)

(define-private (cleanup-single-key 
  (key (string-ascii 64))
  (prev-result (response uint uint))
)
  (if (is-data-expired key)
    (begin
      (map-delete storage-data { key: key })
      (var-set total-entries (- (var-get total-entries) u1))
      prev-result
    )
    prev-result
  )
)

;; Enhanced Read-only Functions

;; Get data with access control
(define-read-only (get-data-secure (key (string-ascii 64)))
  (match (map-get? storage-data { key: key })
    data-entry (begin
      ;; Check expiry
      (asserts! (not (is-data-expired key)) ERR_DATA_EXPIRED)
      
      ;; Check access level
      (if (is-eq (get access-level data-entry) ACCESS_RESTRICTED)
        ;; Check if user has access to restricted data
        (match (map-get? data-access { key: key, user: tx-sender })
          access-entry (if (get granted access-entry) 
            (ok (some data-entry))
            ERR_UNAUTHORIZED
          )
          ERR_UNAUTHORIZED
        )
        (ok (some data-entry))
      )
    )
    (ok none)
  )
)

;; Get data history
(define-read-only (get-data-version (key (string-ascii 64)) (version uint))
  (map-get? data-history { key: key, version: version })
)

;; Get freeze information
(define-read-only (get-freeze-info)
  {
    frozen: (var-get storage-frozen),
    reason: (var-get freeze-reason),
    timestamp: (var-get freeze-timestamp)
  }
)

;; Get user permissions
(define-read-only (get-user-info (user principal))
  (map-get? user-permissions { user: user })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-entries: (var-get total-entries),
    max-entries: (var-get max-entries),
    storage-frozen: (var-get storage-frozen),
    contract-paused: (var-get contract-paused),
    backup-enabled: (var-get backup-enabled)
  }
)

;; Search by tags
(define-read-only (has-tag (key (string-ascii 64)) (tag (string-ascii 32)))
  (match (map-get? storage-data { key: key })
    data-entry (is-some (index-of (get tags data-entry) tag))
    false
  )
)

;; Get event logs
(define-read-only (get-event-log (event-id uint))
  (map-get? event-logs { event-id: event-id })
)

;; Get backup info
(define-read-only (get-backup-info (backup-id uint))
  (map-get? backup-data { backup-id: backup-id })
)

;; Original simple functions (maintained for compatibility)
(define-public (store-data (key (string-ascii 64)) (value (string-utf8 512)))
  (store-data-advanced key value ACCESS_PUBLIC none (list))
)

(define-public (update-data (key (string-ascii 64)) (new-value (string-utf8 512)))
  (update-data-versioned key new-value)
)

(define-public (delete-data (key (string-ascii 64)))
  (begin
    (asserts! (not (var-get storage-frozen)) ERR_STORAGE_FROZEN)
    (asserts! (has-write-permission tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? storage-data { key: key })) ERR_KEY_NOT_FOUND)
    
    (map-delete storage-data { key: key })
    (var-set total-entries (- (var-get total-entries) u1))
    (log-event "DELETE" u"Data deleted successfully")
    (ok true)
  )
)

(define-public (emergency-freeze)
  (emergency-freeze-with-reason u"Emergency freeze activated")
)

(define-public (unfreeze-storage)
  (begin
    (asserts! (is-contract-owner) ERR_UNAUTHORIZED)
    
    (var-set storage-frozen false)
    (var-set freeze-reason u"")
    (var-set freeze-timestamp u0)
    
    (log-event "STORAGE_UNFROZEN" u"Storage operations resumed")
    (ok true)
  )
)

;; Original read-only functions
(define-read-only (get-data (key (string-ascii 64)))
  (map-get? storage-data { key: key })
)

(define-read-only (get-value (key (string-ascii 64)))
  (match (map-get? storage-data { key: key })
    data-entry (some (get value data-entry))
    none
  )
)

(define-read-only (get-updated-at (key (string-ascii 64)))
  (match (map-get? storage-data { key: key })
    data-entry (some (get updated-at data-entry))
    none
  )
)

(define-read-only (is-storage-frozen)
  (var-get storage-frozen)
)

(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)

(define-read-only (is-owner (address principal))
  (is-eq address CONTRACT_OWNER)
)