;; Multi-Send with Access Control Smart Contract
;; Allows authorized users to send STX to multiple recipients in a single transaction

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_RECIPIENT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_TRANSFER_FAILED (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_EMPTY_RECIPIENTS (err u105))
(define-constant ERR_ALREADY_WHITELISTED (err u106))
(define-constant ERR_NOT_WHITELISTED (err u107))

;; Data Variables
(define-data-var contract-admin principal CONTRACT_OWNER)

;; Data Maps
(define-map whitelist principal bool)
(define-map admin-list principal bool)

;; Initialize contract owner as admin and whitelisted user
(map-set admin-list CONTRACT_OWNER true)
(map-set whitelist CONTRACT_OWNER true)

;; Read-only functions

;; Check if a user is whitelisted
(define-read-only (is-whitelisted (user principal))
  (default-to false (map-get? whitelist user))
)

;; Check if a user is an admin
(define-read-only (is-admin (user principal))
  (default-to false (map-get? admin-list user))
)

;; Get current contract admin
(define-read-only (get-contract-admin)
  (var-get contract-admin)
)

;; Check if caller is authorized (admin or whitelisted)
(define-read-only (is-authorized (user principal))
  (or (is-admin user) (is-whitelisted user))
)

;; Private functions

;; Validate that caller is authorized
(define-private (assert-authorized)
  (if (is-authorized tx-sender)
    (ok true)
    ERR_UNAUTHORIZED
  )
)

;; Validate that caller is admin
(define-private (assert-admin)
  (if (is-admin tx-sender)
    (ok true)
    ERR_UNAUTHORIZED
  )
)

;; Send STX to a single recipient
(define-private (send-stx-to-recipient (recipient-data {recipient: principal, amount: uint}))
  (let (
    (recipient (get recipient recipient-data))
    (amount (get amount recipient-data))
  )
    (if (> amount u0)
      (match (stx-transfer? amount tx-sender recipient)
        success (ok amount)
        error (err error)
      )
      ERR_INVALID_AMOUNT
    )
  )
)

;; Public functions

;; Add user to whitelist (admin only)
(define-public (add-to-whitelist (user principal))
  (begin
    (try! (assert-admin))
    (if (is-whitelisted user)
      ERR_ALREADY_WHITELISTED
      (begin
        (map-set whitelist user true)
        (ok true)
      )
    )
  )
)

;; Remove user from whitelist (admin only)
(define-public (remove-from-whitelist (user principal))
  (begin
    (try! (assert-admin))
    (if (is-whitelisted user)
      (begin
        (map-delete whitelist user)
        (ok true)
      )
      ERR_NOT_WHITELISTED
    )
  )
)

;; Add admin (current admin only)
(define-public (add-admin (new-admin principal))
  (begin
    (try! (assert-admin))
    (map-set admin-list new-admin true)
    (ok true)
  )
)

;; Remove admin (current admin only, cannot remove themselves)
(define-public (remove-admin (admin-to-remove principal))
  (begin
    (try! (assert-admin))
    (if (is-eq admin-to-remove tx-sender)
      ERR_UNAUTHORIZED
      (begin
        (map-delete admin-list admin-to-remove)
        (ok true)
      )
    )
  )
)

;; Transfer contract admin role (current admin only)
(define-public (transfer-admin (new-admin principal))
  (begin
    (try! (assert-admin))
    (var-set contract-admin new-admin)
    (map-set admin-list new-admin true)
    (ok true)
  )
)

;; Multi-send STX to multiple recipients (authorized users only)
(define-public (multi-send-stx (recipients (list 50 {recipient: principal, amount: uint})))
  (begin
    ;; Check if caller is authorized
    (try! (assert-authorized))
    
    ;; Check if recipients list is not empty
    (if (is-eq (len recipients) u0)
      ERR_EMPTY_RECIPIENTS
      (begin
        ;; Calculate total amount needed
        (let (
          (total-amount (fold + (map get-amount recipients) u0))
        )
          ;; Check if sender has sufficient balance
          (if (>= (stx-get-balance tx-sender) total-amount)
            ;; Execute transfers
            (begin
              (try! (fold check-and-send recipients (ok u0)))
              (ok {
                success: true,
                total-sent: total-amount,
                recipients-count: (len recipients)
              })
            )
            ERR_INSUFFICIENT_BALANCE
          )
        )
      )
    )
  )
)

;; Helper function to get amount from recipient data
(define-private (get-amount (recipient-data {recipient: principal, amount: uint}))
  (get amount recipient-data)
)

;; Helper function to check and send STX (used with fold)
(define-private (check-and-send 
  (recipient-data {recipient: principal, amount: uint})
  (previous-result (response uint uint))
)
  (match previous-result
    success-val (match (send-stx-to-recipient recipient-data)
                  transfer-success (ok (+ success-val transfer-success))
                  transfer-error (err transfer-error)
                )
    error-val (err error-val)
  )
)

;; Batch multi-send with different amounts (authorized users only)
;; Alternative implementation with explicit error handling
(define-public (batch-transfer (transfers (list 50 {to: principal, amount: uint})))
  (begin
    (try! (assert-authorized))
    (if (is-eq (len transfers) u0)
      ERR_EMPTY_RECIPIENTS
      (ok (map execute-transfer transfers))
    )
  )
)

;; Execute single transfer with error handling
(define-private (execute-transfer (transfer-data {to: principal, amount: uint}))
  (let (
    (recipient (get to transfer-data))
    (amount (get amount transfer-data))
  )
    (if (and (> amount u0) (not (is-eq recipient tx-sender)))
      (match (stx-transfer? amount tx-sender recipient)
        success true
        error false
      )
      false
    )
  )
)

;; Emergency functions

;; Emergency stop - remove all whitelist access (admin only)
(define-public (emergency-clear-whitelist)
  (begin
    (try! (assert-admin))
    ;; Note: In a real implementation, you'd need to track all whitelisted users
    ;; and remove them individually, as Clarity doesn't have a way to clear all map entries
    (ok true)
  )
)

;; Get contract info
(define-read-only (get-contract-info)
  {
    admin: (var-get contract-admin),
    sender-authorized: (is-authorized tx-sender),
    sender-is-admin: (is-admin tx-sender),
    sender-whitelisted: (is-whitelisted tx-sender)
  }
)