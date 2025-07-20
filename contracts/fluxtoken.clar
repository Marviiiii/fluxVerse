;; FluxToken - Mock implementation for local development
;; SIP-010 compliant token for CDP system

;; Error constants
(define-constant ERR-UNAUTHORIZED u401)
(define-constant ERR-NOT-TOKEN-OWNER u402)
(define-constant ERR-INSUFFICIENT-BALANCE u403)
(define-constant ERR-INVALID-AMOUNT u404)
(define-constant ERR-INVALID-PRINCIPAL u405)

;; Token constants
(define-constant TOKEN-NAME "FluxToken")
(define-constant TOKEN-SYMBOL "FLUX")
(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-URI "https://localhost:3000/token-metadata.json")

;; Maximum supply to prevent overflow (2^64 - 1, but using a reasonable limit)
(define-constant MAX-SUPPLY u340282366920938463463374607431768211455)

;; Contract owner (initially the deployer, can be changed to CDP contract)
(define-data-var contract-owner principal tx-sender)

;; Token data
(define-data-var total-supply uint u0)
(define-map balances principal uint)
(define-map allowances {owner: principal, spender: principal} uint)

;; Input validation helpers
(define-private (is-valid-amount (amount uint))
  (and (> amount u0) (<= amount MAX-SUPPLY))
)

(define-private (is-valid-principal (account principal))
  (not (is-eq account 'SP000000000000000000002Q6VF78))
)

(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (asserts! (>= result a) (err ERR-INVALID-AMOUNT))
    (ok result)
  )
)

(define-private (safe-sub (a uint) (b uint))
  (begin
    (asserts! (>= a b) (err ERR-INSUFFICIENT-BALANCE))
    (ok (- a b))
  )
)

;; SIP-010 Standard Functions

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (or (is-eq tx-sender sender) (is-eq contract-caller sender)) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-amount amount) (err ERR-INVALID-AMOUNT))
    (asserts! (is-valid-principal sender) (err ERR-INVALID-PRINCIPAL))
    (asserts! (is-valid-principal recipient) (err ERR-INVALID-PRINCIPAL))
    (asserts! (not (is-eq sender recipient)) (err ERR-INVALID-PRINCIPAL))
    
    (let ((sender-balance (get-balance sender))
          (recipient-balance (get-balance recipient)))
      (asserts! (>= sender-balance amount) (err ERR-INSUFFICIENT-BALANCE))
      (let ((new-sender-balance (unwrap! (safe-sub sender-balance amount) (err ERR-INSUFFICIENT-BALANCE)))
            (new-recipient-balance (unwrap! (safe-add recipient-balance amount) (err ERR-INVALID-AMOUNT))))
        (map-set balances sender new-sender-balance)
        (map-set balances recipient new-recipient-balance)
        (print {action: "transfer", sender: sender, recipient: recipient, amount: amount, memo: memo})
        (ok true)
      )
    )
  )
)

(define-read-only (get-name)
  (ok TOKEN-NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

(define-read-only (get-balance (account principal))
  (if (is-valid-principal account)
    (default-to u0 (map-get? balances account))
    u0
  )
)

(define-read-only (get-total-supply)
  (ok (var-get total-supply))
)

(define-read-only (get-token-uri)
  (ok (some TOKEN-URI))
)

;; Additional SIP-010 functions for allowances (optional but useful)

(define-public (approve (spender principal) (amount uint))
  (begin
    (asserts! (is-valid-principal spender) (err ERR-INVALID-PRINCIPAL))
    (asserts! (not (is-eq tx-sender spender)) (err ERR-INVALID-PRINCIPAL))
    (asserts! (<= amount MAX-SUPPLY) (err ERR-INVALID-AMOUNT))
    
    (map-set allowances {owner: tx-sender, spender: spender} amount)
    (print {action: "approve", owner: tx-sender, spender: spender, amount: amount})
    (ok true)
  )
)

(define-read-only (get-allowance (owner principal) (spender principal))
  (if (and (is-valid-principal owner) (is-valid-principal spender))
    (default-to u0 (map-get? allowances {owner: owner, spender: spender}))
    u0
  )
)

(define-public (transfer-from (amount uint) (owner principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-valid-amount amount) (err ERR-INVALID-AMOUNT))
    (asserts! (is-valid-principal owner) (err ERR-INVALID-PRINCIPAL))
    (asserts! (is-valid-principal recipient) (err ERR-INVALID-PRINCIPAL))
    (asserts! (not (is-eq owner recipient)) (err ERR-INVALID-PRINCIPAL))
    
    (let ((allowance (get-allowance owner tx-sender))
          (owner-balance (get-balance owner))
          (recipient-balance (get-balance recipient)))
      (asserts! (>= allowance amount) (err ERR-UNAUTHORIZED))
      (asserts! (>= owner-balance amount) (err ERR-INSUFFICIENT-BALANCE))
      
      (let ((new-allowance (unwrap! (safe-sub allowance amount) (err ERR-UNAUTHORIZED)))
            (new-owner-balance (unwrap! (safe-sub owner-balance amount) (err ERR-INSUFFICIENT-BALANCE)))
            (new-recipient-balance (unwrap! (safe-add recipient-balance amount) (err ERR-INVALID-AMOUNT))))
        (map-set allowances {owner: owner, spender: tx-sender} new-allowance)
        (map-set balances owner new-owner-balance)
        (map-set balances recipient new-recipient-balance)
        (print {action: "transfer-from", owner: owner, recipient: recipient, amount: amount, memo: memo})
        (ok true)
      )
    )
  )
)

;; CDP-specific functions (mint/burn)

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-amount amount) (err ERR-INVALID-AMOUNT))
    (asserts! (is-valid-principal recipient) (err ERR-INVALID-PRINCIPAL))
    
    (let ((current-supply (var-get total-supply))
          (recipient-balance (get-balance recipient)))
      (let ((new-supply (unwrap! (safe-add current-supply amount) (err ERR-INVALID-AMOUNT)))
            (new-balance (unwrap! (safe-add recipient-balance amount) (err ERR-INVALID-AMOUNT))))
        (asserts! (<= new-supply MAX-SUPPLY) (err ERR-INVALID-AMOUNT))
        (map-set balances recipient new-balance)
        (var-set total-supply new-supply)
        (print {action: "mint", recipient: recipient, amount: amount})
        (ok true)
      )
    )
  )
)

(define-public (burn (amount uint) (sender principal))
  (begin
    (asserts! (or (is-eq tx-sender sender) (is-eq tx-sender (var-get contract-owner))) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-amount amount) (err ERR-INVALID-AMOUNT))
    (asserts! (is-valid-principal sender) (err ERR-INVALID-PRINCIPAL))
    
    (let ((sender-balance (get-balance sender))
          (current-supply (var-get total-supply)))
      (let ((new-balance (unwrap! (safe-sub sender-balance amount) (err ERR-INSUFFICIENT-BALANCE)))
            (new-supply (unwrap! (safe-sub current-supply amount) (err ERR-INSUFFICIENT-BALANCE))))
        (map-set balances sender new-balance)
        (var-set total-supply new-supply)
        (print {action: "burn", sender: sender, amount: amount})
        (ok true)
      )
    )
  )
)

;; Admin functions

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-principal new-owner) (err ERR-INVALID-PRINCIPAL))
    (asserts! (not (is-eq tx-sender new-owner)) (err ERR-INVALID-PRINCIPAL))
    
    (let ((old-owner (var-get contract-owner)))
      (var-set contract-owner new-owner)
      (print {action: "set-contract-owner", old-owner: old-owner, new-owner: new-owner})
      (ok true)
    )
  )
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Utility functions for testing

(define-public (mint-for-testing (amount uint))
  (begin
    (asserts! (is-valid-amount amount) (err ERR-INVALID-AMOUNT))
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-PRINCIPAL))
    
    (let ((current-balance (get-balance tx-sender))
          (current-supply (var-get total-supply)))
      (let ((new-balance (unwrap! (safe-add current-balance amount) (err ERR-INVALID-AMOUNT)))
            (new-supply (unwrap! (safe-add current-supply amount) (err ERR-INVALID-AMOUNT))))
        (asserts! (<= new-supply MAX-SUPPLY) (err ERR-INVALID-AMOUNT))
        (map-set balances tx-sender new-balance)
        (var-set total-supply new-supply)
        (print {action: "mint-for-testing", recipient: tx-sender, amount: amount})
        (ok true)
      )
    )
  )
)

;; Read-only functions for debugging

(define-read-only (get-token-info)
  {
    name: TOKEN-NAME,
    symbol: TOKEN-SYMBOL,
    decimals: TOKEN-DECIMALS,
    total-supply: (var-get total-supply),
    contract-owner: (var-get contract-owner)
  }
)

(define-read-only (get-balance-info (account principal))
  (if (is-valid-principal account)
    {
      account: account,
      balance: (get-balance account),
      total-supply: (var-get total-supply)
    }
    {
      account: account,
      balance: u0,
      total-supply: (var-get total-supply)
    }
  )
)