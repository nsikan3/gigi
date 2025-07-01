;; Enhanced FileVersioning Smart Contract
;; Purpose: Advanced file versioning system with collaboration and metadata features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-FILE-NOT-FOUND (err u101))
(define-constant ERR-VERSION-NOT-FOUND (err u102))
(define-constant ERR-INVALID-VERSION (err u103))
(define-constant ERR-EMPTY-HASH (err u104))
(define-constant ERR-INVALID-PERMISSION (err u105))
(define-constant ERR-USER-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-EXISTS (err u107))
(define-constant ERR-INVALID-TAG (err u108))
(define-constant ERR-BRANCH-NOT-FOUND (err u109))
(define-constant ERR-INVALID-BRANCH (err u110))
(define-constant ERR-COMMENT-NOT-FOUND (err u111))
(define-constant ERR-INVALID-STATUS (err u112))

;; Permission levels
(define-constant PERMISSION-READ u1)
(define-constant PERMISSION-WRITE u2)
(define-constant PERMISSION-ADMIN u3)

;; File status constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-ARCHIVED u2)
(define-constant STATUS-DELETED u3)
(define-constant STATUS-LOCKED u4)

;; Data structures
(define-map files
  { file-id: (string-ascii 64) }
  {
    owner: principal,
    current-version: uint,
    created-at: uint,
    updated-at: uint,
    file-name: (string-ascii 256),
    file-type: (string-ascii 32),
    status: uint,
    total-size: uint,
    current-branch: (string-ascii 32),
    is-public: bool,
    encryption-key: (optional (string-ascii 64))
  }
)

(define-map file-versions
  { file-id: (string-ascii 64), version: uint }
  {
    ipfs-hash: (string-ascii 64),
    description: (string-ascii 256),
    created-by: principal,
    created-at: uint,
    file-size: uint,
    branch: (string-ascii 32),
    parent-version: (optional uint),
    checksum: (string-ascii 64),
    is-major: bool,
    tags: (list 10 (string-ascii 32))
  }
)

;; Collaboration features
(define-map file-permissions
  { file-id: (string-ascii 64), user: principal }
  { permission-level: uint, granted-by: principal, granted-at: uint }
)

(define-map file-collaborators
  { file-id: (string-ascii 64) }
  { collaborators: (list 50 principal), collaborator-count: uint }
)

;; Branching system
(define-map file-branches
  { file-id: (string-ascii 64), branch-name: (string-ascii 32) }
  {
    created-by: principal,
    created-at: uint,
    latest-version: uint,
    parent-branch: (optional (string-ascii 32)),
    is-active: bool
  }
)

;; Comments and annotations
(define-map version-comments
  { file-id: (string-ascii 64), version: uint, comment-id: uint }
  {
    author: principal,
    content: (string-ascii 500),
    created-at: uint,
    is-resolved: bool
  }
)

(define-map file-comment-counter
  { file-id: (string-ascii 64) }
  { next-comment-id: uint }
)

;; Tags and metadata
(define-map file-tags
  { file-id: (string-ascii 64) }
  { tags: (list 20 (string-ascii 32)) }
)

(define-map file-metadata
  { file-id: (string-ascii 64) }
  { 
    custom-fields: (list 10 { key: (string-ascii 64), value: (string-ascii 256) }),
    field-count: uint
  }
)

;; Activity log
(define-map file-activity
  { file-id: (string-ascii 64), activity-id: uint }
  {
    action: (string-ascii 64),
    user: principal,
    timestamp: uint,
    details: (string-ascii 256)
  }
)

(define-map file-activity-counter
  { file-id: (string-ascii 64) }
  { next-activity-id: uint }
)

;; Storage counters
(define-map file-version-counter
  { file-id: (string-ascii 64) }
  { next-version: uint }
)

;; Public functions

;; Enhanced file creation
(define-public (create-file (file-id (string-ascii 64)) 
                           (file-name (string-ascii 256))
                           (file-type (string-ascii 32))
                           (ipfs-hash (string-ascii 64)) 
                           (description (string-ascii 256))
                           (file-size uint)
                           (checksum (string-ascii 64))
                           (is-public bool)
                           (initial-tags (list 10 (string-ascii 32))))
  (let ((caller tx-sender)
        (current-time block-height))
    
    (asserts! (is-none (map-get? files { file-id: file-id })) ERR-ALREADY-EXISTS)
    (asserts! (> (len ipfs-hash) u0) ERR-EMPTY-HASH)
    
    ;; Create file entry
    (map-set files
      { file-id: file-id }
      {
        owner: caller,
        current-version: u1,
        created-at: current-time,
        updated-at: current-time,
        file-name: file-name,
        file-type: file-type,
        status: STATUS-ACTIVE,
        total-size: file-size,
        current-branch: "main",
        is-public: is-public,
        encryption-key: none
      }
    )
    
    ;; Create first version
    (map-set file-versions
      { file-id: file-id, version: u1 }
      {
        ipfs-hash: ipfs-hash,
        description: description,
        created-by: caller,
        created-at: current-time,
        file-size: file-size,
        branch: "main",
        parent-version: none,
        checksum: checksum,
        is-major: true,
        tags: initial-tags
      }
    )
    
    ;; Initialize main branch
    (map-set file-branches
      { file-id: file-id, branch-name: "main" }
      {
        created-by: caller,
        created-at: current-time,
        latest-version: u1,
        parent-branch: none,
        is-active: true
      }
    )
    
    ;; Initialize counters
    (map-set file-version-counter { file-id: file-id } { next-version: u2 })
    (map-set file-comment-counter { file-id: file-id } { next-comment-id: u1 })
    (map-set file-activity-counter { file-id: file-id } { next-activity-id: u1 })
    
    ;; Log activity
    (unwrap-panic (log-activity file-id "FILE_CREATED" caller "Initial file creation"))
    
    (ok file-id)
  )
)

;; Enhanced version addition with branching
(define-public (add-version (file-id (string-ascii 64)) 
                           (ipfs-hash (string-ascii 64)) 
                           (description (string-ascii 256))
                           (file-size uint)
                           (checksum (string-ascii 64))
                           (branch-name (string-ascii 32))
                           (is-major bool)
                           (tags (list 10 (string-ascii 32))))
  (let ((caller tx-sender)
        (current-time block-height))
    
    (asserts! (can-write-file file-id caller) ERR-NOT-AUTHORIZED)
    (asserts! (> (len ipfs-hash) u0) ERR-EMPTY-HASH)
    
    (let ((file-info (unwrap! (map-get? files { file-id: file-id }) ERR-FILE-NOT-FOUND))
          (version-counter (unwrap! (map-get? file-version-counter { file-id: file-id }) ERR-FILE-NOT-FOUND))
          (new-version (get next-version version-counter))
          (branch-info (unwrap! (map-get? file-branches { file-id: file-id, branch-name: branch-name }) ERR-BRANCH-NOT-FOUND)))
      
      ;; Create new version
      (map-set file-versions
        { file-id: file-id, version: new-version }
        {
          ipfs-hash: ipfs-hash,
          description: description,
          created-by: caller,
          created-at: current-time,
          file-size: file-size,
          branch: branch-name,
          parent-version: (some (get latest-version branch-info)),
          checksum: checksum,
          is-major: is-major,
          tags: tags
        }
      )
      
      ;; Update file info
      (map-set files
        { file-id: file-id }
        (merge file-info {
          current-version: new-version,
          updated-at: current-time,
          total-size: (+ (get total-size file-info) file-size)
        })
      )
      
      ;; Update branch
      (map-set file-branches
        { file-id: file-id, branch-name: branch-name }
        (merge branch-info { latest-version: new-version })
      )
      
      ;; Increment version counter
      (map-set file-version-counter { file-id: file-id } { next-version: (+ new-version u1) })
      
      ;; Log activity
      (unwrap-panic (log-activity file-id "VERSION_ADDED" caller (concat "Added version " (uint-to-string new-version))))
      
      (ok new-version)
    )
  )
)

;; Create new branch
(define-public (create-branch (file-id (string-ascii 64)) 
                             (branch-name (string-ascii 32))
                             (parent-branch (string-ascii 32)))
  (let ((caller tx-sender)
        (current-time block-height))
    
    (asserts! (can-write-file file-id caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? file-branches { file-id: file-id, branch-name: branch-name })) ERR-ALREADY-EXISTS)
    
    (let ((parent-branch-info (unwrap! (map-get? file-branches { file-id: file-id, branch-name: parent-branch }) ERR-BRANCH-NOT-FOUND)))
      
      (map-set file-branches
        { file-id: file-id, branch-name: branch-name }
        {
          created-by: caller,
          created-at: current-time,
          latest-version: (get latest-version parent-branch-info),
          parent-branch: (some parent-branch),
          is-active: true
        }
      )
      
      (unwrap-panic (log-activity file-id "BRANCH_CREATED" caller (concat "Created branch " branch-name)))
      (ok branch-name)
    )
  )
)

;; Grant file permissions
(define-public (grant-permission (file-id (string-ascii 64)) 
                                (user principal) 
                                (permission-level uint))
  (let ((caller tx-sender)
        (current-time block-height))
    
    (asserts! (can-admin-file file-id caller) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= permission-level PERMISSION-READ) (<= permission-level PERMISSION-ADMIN)) ERR-INVALID-PERMISSION)
    
    (map-set file-permissions
      { file-id: file-id, user: user }
      { permission-level: permission-level, granted-by: caller, granted-at: current-time }
    )
    
    ;; Add to collaborators list
    (add-collaborator file-id user)
    
    (unwrap-panic (log-activity file-id "PERMISSION_GRANTED" caller "Granted permission to user"))
    (ok true)
  )
)

;; Add comment to version
(define-public (add-comment (file-id (string-ascii 64)) 
                           (version uint)
                           (content (string-ascii 500)))
  (let ((caller tx-sender)
        (current-time block-height))
    
    (asserts! (can-read-file file-id caller) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (map-get? file-versions { file-id: file-id, version: version })) ERR-VERSION-NOT-FOUND)
    
    (let ((comment-counter (unwrap! (map-get? file-comment-counter { file-id: file-id }) ERR-FILE-NOT-FOUND))
          (comment-id (get next-comment-id comment-counter)))
      
      (map-set version-comments
        { file-id: file-id, version: version, comment-id: comment-id }
        {
          author: caller,
          content: content,
          created-at: current-time,
          is-resolved: false
        }
      )
      
      (map-set file-comment-counter { file-id: file-id } { next-comment-id: (+ comment-id u1) })
      
      (unwrap-panic (log-activity file-id "COMMENT_ADDED" caller "Added comment"))
      (ok comment-id)
    )
  )
)

;; Set file status
(define-public (set-file-status (file-id (string-ascii 64)) (new-status uint))
  (let ((caller tx-sender))
    (asserts! (can-admin-file file-id caller) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-status STATUS-ACTIVE) (<= new-status STATUS-LOCKED)) ERR-INVALID-STATUS)
    
    (let ((file-info (unwrap! (map-get? files { file-id: file-id }) ERR-FILE-NOT-FOUND)))
      (map-set files
        { file-id: file-id }
        (merge file-info { status: new-status })
      )
      
      (unwrap-panic (log-activity file-id "STATUS_CHANGED" caller (concat "Status changed to " (uint-to-string new-status))))
      (ok true)
    )
  )
)

;; Add custom metadata
(define-public (add-metadata (file-id (string-ascii 64)) 
                            (key (string-ascii 64)) 
                            (value (string-ascii 256)))
  (let ((caller tx-sender))
    (asserts! (can-write-file file-id caller) ERR-NOT-AUTHORIZED)
    
    (let ((metadata (default-to { custom-fields: (list), field-count: u0 } 
                                (map-get? file-metadata { file-id: file-id })))
          (new-field { key: key, value: value }))
      
      (map-set file-metadata
        { file-id: file-id }
        {
          custom-fields: (unwrap-panic (as-max-len? (append (get custom-fields metadata) new-field) u10)),
          field-count: (+ (get field-count metadata) u1)
        }
      )
      
      (ok true)
    )
  )
)

;; Helper functions for permissions
(define-private (can-read-file (file-id (string-ascii 64)) (user principal))
  (let ((file-info (map-get? files { file-id: file-id })))
    (match file-info
      file-data (or
        (is-eq user (get owner file-data))
        (get is-public file-data)
        (match (map-get? file-permissions { file-id: file-id, user: user })
          permission (>= (get permission-level permission) PERMISSION-READ)
          false
        )
      )
      false
    )
  )
)

(define-private (can-write-file (file-id (string-ascii 64)) (user principal))
  (let ((file-info (map-get? files { file-id: file-id })))
    (match file-info
      file-data (or
        (is-eq user (get owner file-data))
        (match (map-get? file-permissions { file-id: file-id, user: user })
          permission (>= (get permission-level permission) PERMISSION-WRITE)
          false
        )
      )
      false
    )
  )
)

(define-private (can-admin-file (file-id (string-ascii 64)) (user principal))
  (let ((file-info (map-get? files { file-id: file-id })))
    (match file-info
      file-data (or
        (is-eq user (get owner file-data))
        (match (map-get? file-permissions { file-id: file-id, user: user })
          permission (>= (get permission-level permission) PERMISSION-ADMIN)
          false
        )
      )
      false
    )
  )
)

(define-private (add-collaborator (file-id (string-ascii 64)) (user principal))
  (let ((collaborators (default-to { collaborators: (list), collaborator-count: u0 } 
                                  (map-get? file-collaborators { file-id: file-id }))))
    (map-set file-collaborators
      { file-id: file-id }
      {
        collaborators: (unwrap-panic (as-max-len? (append (get collaborators collaborators) user) u50)),
        collaborator-count: (+ (get collaborator-count collaborators) u1)
      }
    )
  )
)

(define-private (log-activity (file-id (string-ascii 64)) 
                             (action (string-ascii 64)) 
                             (user principal) 
                             (details (string-ascii 256)))
  (let ((activity-counter (unwrap! (map-get? file-activity-counter { file-id: file-id }) ERR-FILE-NOT-FOUND))
        (activity-id (get next-activity-id activity-counter))
        (current-time block-height))
    
    (map-set file-activity
      { file-id: file-id, activity-id: activity-id }
      {
        action: action,
        user: user,
        timestamp: current-time,
        details: details
      }
    )
    
    (map-set file-activity-counter { file-id: file-id } { next-activity-id: (+ activity-id u1) })
    (ok activity-id)
  )
)

;; Read-only functions

;; Get enhanced file information
(define-read-only (get-file-info (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

;; Get version information with enhanced details
(define-read-only (get-version-info (file-id (string-ascii 64)) (version uint))
  (map-get? file-versions { file-id: file-id, version: version })
)

;; Get branch information
(define-read-only (get-branch-info (file-id (string-ascii 64)) (branch-name (string-ascii 32)))
  (map-get? file-branches { file-id: file-id, branch-name: branch-name })
)

;; Get user permissions
(define-read-only (get-user-permission (file-id (string-ascii 64)) (user principal))
  (map-get? file-permissions { file-id: file-id, user: user })
)

;; Get file collaborators
(define-read-only (get-collaborators (file-id (string-ascii 64)))
  (map-get? file-collaborators { file-id: file-id })
)

;; Get version comments
(define-read-only (get-comment (file-id (string-ascii 64)) (version uint) (comment-id uint))
  (map-get? version-comments { file-id: file-id, version: version, comment-id: comment-id })
)

;; Get file metadata
(define-read-only (get-file-metadata (file-id (string-ascii 64)))
  (map-get? file-metadata { file-id: file-id })
)

;; Get activity log entry
(define-read-only (get-activity (file-id (string-ascii 64)) (activity-id uint))
  (map-get? file-activity { file-id: file-id, activity-id: activity-id })
)

;; Get version count
(define-read-only (get-version-count (file-id (string-ascii 64)))
  (match (map-get? file-version-counter { file-id: file-id })
    counter (some (- (get next-version counter) u1))
    none
  )
)

;; Check permissions
(define-read-only (can-user-read (file-id (string-ascii 64)) (user principal))
  (can-read-file file-id user)
)

(define-read-only (can-user-write (file-id (string-ascii 64)) (user principal))
  (can-write-file file-id user)
)

(define-read-only (can-user-admin (file-id (string-ascii 64)) (user principal))
  (can-admin-file file-id user)
)

;; Utility functions
(define-read-only (uint-to-string (value uint))
  (if (is-eq value u0) "0"
  (if (is-eq value u1) "1"
  (if (is-eq value u2) "2"
  (if (is-eq value u3) "3"
  (if (is-eq value u4) "4"
  (if (is-eq value u5) "5"
  (if (is-eq value u6) "6"
  (if (is-eq value u7) "7"
  (if (is-eq value u8) "8"
  (if (is-eq value u9) "9"
  "unknown"))))))))))
)