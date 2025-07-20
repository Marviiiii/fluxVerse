;; FluxVerse CDP (Collateralized Debt Position) Contract
;; Allows users to deposit STX as collateral and borrow FLUX tokens

;; Error constants
(define-constant ERR-NO-VAULT u100)
(define-constant ERR-UNDERCOLLATERALIZED u101)
(define-constant ERR-BAD-RATIO u102)
(define-constant ERR-NO-DEBT u103)
(define-constant ERR-NOT-ENOUGH-COLLATERAL u104)
(define-constant ERR-VAULT-EXISTS u105)
(define-constant ERR-INSUFFICIENT-AMOUNT u106)
(define-constant ERR-TOKEN-TRANSFER-FAILED u107)
(define-constant ERR-UNAUTHORIZED u108)
(define-constant ERR-PRICE-UNAVAILABLE u109)
(define-constant ERR-INVALID-INPUT u110)
(define-constant ERR-ARITHMETIC-OVERFLOW u111)

;; System parameters
(define-constant MIN-COLLATERAL-RATIO u150) ;; 150%
(define-constant LIQUIDATION-RATIO u130)    ;; 130%
(define-constant LIQUIDATION-BONUS u10)     ;; 10%
(define-constant INTEREST-RATE-BP u500)     ;; 5.00% annualized, in basis points
(define-constant BLOCKS-PER-YEAR u52560)    ;; Assuming ~10 min blocks
(define-constant PRECISION u1000000)        ;; 6 decimal precision for calculations

;; Maximum values to prevent overflow
(define-constant MAX-UINT u340282366920938463463374607431768211455)
(define-constant MAX-COLLATERAL u1000000000000000) ;; 1 billion STX max
(define-constant MAX-DEBT u1000000000000000)       ;; 1 billion FLUX max
(define-constant MAX-PRICE u1000000000000)         ;; $1 million max price

;; FluxToken contract reference - using local contract
(define-constant FLUX-TOKEN .fluxtoken)

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Price data (in production, this would come from an oracle)
(define-data-var stx-price uint u1000000) ;; $1.00 in micro-dollars (6 decimals)
(define-data-var flux-price uint u1000000) ;; $1.00 in micro-dollars (6 decimals)

;; Vault data structure
(define-map vaults principal
  {
    collateral: uint, ;; in micro-STX
    debt: uint,       ;; in FLUX (6 decimals)
    last-block: uint  ;; last interest calculation block
  }
)

;; Input validation helpers
(define-private (is-valid-amount (amount uint))
  (and (> amount u0) (<= amount MAX-COLLATERAL))
)

(define-private (is-valid-debt-amount (amount uint))
  (and (> amount u0) (<= amount MAX-DEBT))
)

(define-private (is-valid-price (price uint))
  (and (> price u0) (<= price MAX-PRICE))
)

(define-private (is-valid-principal (account principal))
  (not (is-eq account 'SP000000000000000000002Q6VF78))
)

;; Safe arithmetic operations
(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (asserts! (>= result a) (err ERR-ARITHMETIC-OVERFLOW))
    (ok result)
  )
)

(define-private (safe-sub (a uint) (b uint))
  (begin
    (asserts! (>= a b) (err ERR-INSUFFICIENT-AMOUNT))
    (ok (- a b))
  )
)

(define-private (safe-mul (a uint) (b uint))
  (if (is-eq a u0)
    (ok u0)
    (let ((result (* a b)))
      (asserts! (is-eq (/ result a) b) (err ERR-ARITHMETIC-OVERFLOW))
      (ok result)
    )
  )
)

(define-private (safe-div (a uint) (b uint))
  (begin
    (asserts! (> b u0) (err ERR-INVALID-INPUT))
    (ok (/ a b))
  )
)

;; Read-only functions

(define-read-only (get-vault (owner principal))
  (if (is-valid-principal owner)
    (map-get? vaults owner)
    none
  )
)

(define-read-only (get-vault-info (owner principal))
  (begin
    (asserts! (is-valid-principal owner) (err ERR-INVALID-INPUT))
    (match (get-vault owner)
      vault (let (
          (updated-vault (unwrap! (calculate-interest-internal vault) (err ERR-ARITHMETIC-OVERFLOW)))
          (collateral-value (unwrap! (get-collateral-value (get collateral updated-vault)) (err ERR-ARITHMETIC-OVERFLOW)))
          (debt-value (get debt updated-vault))
          (ratio (if (> debt-value u0)
                    (unwrap! (safe-div (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) debt-value) (err ERR-ARITHMETIC-OVERFLOW))
                    u0))
        )
        (ok {
          collateral: (get collateral updated-vault),
          debt: (get debt updated-vault),
          collateral-value: collateral-value,
          collateral-ratio: ratio,
          liquidation-price: (if (> (get collateral updated-vault) u0)
                              (unwrap! (safe-div 
                                (unwrap! (safe-mul debt-value LIQUIDATION-RATIO) (err ERR-ARITHMETIC-OVERFLOW))
                                (unwrap! (safe-mul (get collateral updated-vault) u100) (err ERR-ARITHMETIC-OVERFLOW))
                              ) (err ERR-ARITHMETIC-OVERFLOW))
                              u0)
        }))
      (err ERR-NO-VAULT)
    )
  )
)

(define-read-only (get-stx-price)
  (var-get stx-price)
)

(define-read-only (get-flux-price)
  (var-get flux-price)
)

(define-read-only (get-collateral-value (stx-amount uint))
  (begin
    (asserts! (<= stx-amount MAX-COLLATERAL) (err ERR-INVALID-INPUT))
    (let ((price (get-stx-price)))
      (safe-div (unwrap! (safe-mul stx-amount price) (err ERR-ARITHMETIC-OVERFLOW)) PRECISION)
    )
  )
)

(define-read-only (get-flux-token)
  FLUX-TOKEN
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Private helper functions

(define-private (calculate-interest-internal (vault {collateral: uint, debt: uint, last-block: uint}))
  (let (
      (blocks-elapsed (if (>= stacks-block-height (get last-block vault))
                        (- stacks-block-height (get last-block vault))
                        u0))
      (current-debt (get debt vault))
      (interest (if (> current-debt u0)
                   (let ((numerator (unwrap! (safe-mul 
                                      (unwrap! (safe-mul current-debt INTEREST-RATE-BP) (err ERR-ARITHMETIC-OVERFLOW))
                                      blocks-elapsed) (err ERR-ARITHMETIC-OVERFLOW)))
                         (denominator (unwrap! (safe-mul u10000 BLOCKS-PER-YEAR) (err ERR-ARITHMETIC-OVERFLOW))))
                     (unwrap! (safe-div numerator denominator) (err ERR-ARITHMETIC-OVERFLOW)))
                   u0))
      (new-debt (unwrap! (safe-add current-debt interest) (err ERR-ARITHMETIC-OVERFLOW)))
    )
    (begin
      (asserts! (<= new-debt MAX-DEBT) (err ERR-ARITHMETIC-OVERFLOW))
      (ok {
        collateral: (get collateral vault),
        debt: new-debt,
        last-block: stacks-block-height
      })
    )
  )
)

(define-private (update-vault-interest (user principal))
  (begin
    (asserts! (is-valid-principal user) (err ERR-INVALID-INPUT))
    (match (get-vault user)
      vault (let ((updated-vault (unwrap! (calculate-interest-internal vault) (err ERR-ARITHMETIC-OVERFLOW))))
        (map-set vaults user updated-vault)
        (ok updated-vault))
      (err ERR-NO-VAULT)
    )
  )
)

(define-private (check-collateral-ratio (collateral uint) (debt uint))
  (begin
    (asserts! (<= collateral MAX-COLLATERAL) (err ERR-INVALID-INPUT))
    (asserts! (<= debt MAX-DEBT) (err ERR-INVALID-INPUT))
    (if (is-eq debt u0)
        (ok true)
        (let ((collateral-value (unwrap! (get-collateral-value collateral) (err ERR-ARITHMETIC-OVERFLOW)))
              (min-collateral-value (unwrap! (safe-mul debt MIN-COLLATERAL-RATIO) (err ERR-ARITHMETIC-OVERFLOW))))
          (ok (>= (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) min-collateral-value))
        )
    )
  )
)

;; Helper function to get minimum of two values
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Admin functions

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-principal new-owner) (err ERR-INVALID-INPUT))
    (asserts! (not (is-eq tx-sender new-owner)) (err ERR-INVALID-INPUT))
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (update-stx-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-price new-price) (err ERR-INVALID-INPUT))
    (var-set stx-price new-price)
    (ok true)
  )
)

(define-public (update-flux-price (new-price uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
    (asserts! (is-valid-price new-price) (err ERR-INVALID-INPUT))
    (var-set flux-price new-price)
    (ok true)
  )
)

;; Public functions

(define-public (open-vault)
  (begin
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (asserts! (is-none (get-vault tx-sender)) (err ERR-VAULT-EXISTS))
    (map-set vaults tx-sender {
      collateral: u0,
      debt: u0,
      last-block: stacks-block-height
    })
    (ok true)
  )
)

(define-public (deposit-collateral (amount uint))
  (begin
    (asserts! (is-valid-amount amount) (err ERR-INVALID-INPUT))
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success (let ((vault (unwrap! (update-vault-interest tx-sender) (err ERR-NO-VAULT)))
                    (new-collateral (unwrap! (safe-add (get collateral vault) amount) (err ERR-ARITHMETIC-OVERFLOW))))
        (asserts! (<= new-collateral MAX-COLLATERAL) (err ERR-INVALID-INPUT))
        (map-set vaults tx-sender {
          collateral: new-collateral,
          debt: (get debt vault),
          last-block: stacks-block-height
        })
        (ok true)
      )
      error (err ERR-TOKEN-TRANSFER-FAILED)
    )
  )
)

(define-public (withdraw-collateral (amount uint))
  (begin
    (asserts! (is-valid-amount amount) (err ERR-INVALID-INPUT))
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (let ((vault (unwrap! (update-vault-interest tx-sender) (err ERR-NO-VAULT))))
      (asserts! (>= (get collateral vault) amount) (err ERR-NOT-ENOUGH-COLLATERAL))
      (let ((new-collateral (unwrap! (safe-sub (get collateral vault) amount) (err ERR-ARITHMETIC-OVERFLOW))))
        (asserts! (unwrap! (check-collateral-ratio new-collateral (get debt vault)) (err ERR-ARITHMETIC-OVERFLOW)) (err ERR-BAD-RATIO))
        (map-set vaults tx-sender {
          collateral: new-collateral,
          debt: (get debt vault),
          last-block: stacks-block-height
        })
        (match (as-contract (stx-transfer? amount tx-sender tx-sender))
          success (ok true)
          error (err ERR-TOKEN-TRANSFER-FAILED)
        )
      )
    )
  )
)

(define-public (borrow (amount uint))
  (begin
    (asserts! (is-valid-debt-amount amount) (err ERR-INVALID-INPUT))
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (let (
        (vault (unwrap! (update-vault-interest tx-sender) (err ERR-NO-VAULT)))
        (new-debt (unwrap! (safe-add (get debt vault) amount) (err ERR-ARITHMETIC-OVERFLOW)))
      )
      (asserts! (<= new-debt MAX-DEBT) (err ERR-INVALID-INPUT))
      (asserts! (unwrap! (check-collateral-ratio (get collateral vault) new-debt) (err ERR-ARITHMETIC-OVERFLOW)) (err ERR-BAD-RATIO))
      (map-set vaults tx-sender {
        collateral: (get collateral vault),
        debt: new-debt,
        last-block: stacks-block-height
      })
      ;; Mint FLUX tokens to the borrower
      (match (as-contract (contract-call? FLUX-TOKEN mint amount tx-sender))
        success (ok true)
        error (err ERR-TOKEN-TRANSFER-FAILED)
      )
    )
  )
)

(define-public (repay (amount uint))
  (begin
    (asserts! (is-valid-debt-amount amount) (err ERR-INVALID-INPUT))
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (let (
        (vault (unwrap! (update-vault-interest tx-sender) (err ERR-NO-VAULT)))
        (repay-amount (min-uint amount (get debt vault)))
      )
      (asserts! (> repay-amount u0) (err ERR-NO-DEBT))
      ;; Burn FLUX tokens from the borrower
      (match (contract-call? FLUX-TOKEN burn repay-amount tx-sender)
        success (begin
          (let ((new-debt (unwrap! (safe-sub (get debt vault) repay-amount) (err ERR-ARITHMETIC-OVERFLOW))))
            (map-set vaults tx-sender {
              collateral: (get collateral vault),
              debt: new-debt,
              last-block: stacks-block-height
            })
            (ok repay-amount)
          )
        )
        error (err ERR-TOKEN-TRANSFER-FAILED)
      )
    )
  )
)

(define-public (liquidate (target principal))
  (begin
    (asserts! (is-valid-principal target) (err ERR-INVALID-INPUT))
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (asserts! (not (is-eq target tx-sender)) (err ERR-INVALID-INPUT))
    (let (
        (vault (unwrap! (update-vault-interest target) (err ERR-NO-VAULT)))
        (collateral (get collateral vault))
        (debt (get debt vault))
        (collateral-value (unwrap! (get-collateral-value collateral) (err ERR-ARITHMETIC-OVERFLOW)))
      )
      (asserts! (> debt u0) (err ERR-NO-DEBT))
      (let ((liquidation-threshold (unwrap! (safe-mul debt LIQUIDATION-RATIO) (err ERR-ARITHMETIC-OVERFLOW))))
        (asserts! (< (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) liquidation-threshold) (err ERR-UNDERCOLLATERALIZED))
        (let (
            (liquidation-bonus (unwrap! (safe-div (unwrap! (safe-mul collateral LIQUIDATION-BONUS) (err ERR-ARITHMETIC-OVERFLOW)) u100) (err ERR-ARITHMETIC-OVERFLOW)))
            (collateral-to-liquidator (unwrap! (safe-add collateral liquidation-bonus) (err ERR-ARITHMETIC-OVERFLOW)))
          )
          ;; Burn liquidator's FLUX tokens to cover the debt
          (match (contract-call? FLUX-TOKEN burn debt tx-sender)
            success (begin
              ;; Delete the vault
              (map-delete vaults target)
              ;; Transfer collateral + bonus to liquidator
              (match (as-contract (stx-transfer? collateral-to-liquidator tx-sender tx-sender))
                transfer-success (ok {
                  collateral-seized: collateral,
                  debt-repaid: debt,
                  bonus: liquidation-bonus
                })
                transfer-error (err ERR-TOKEN-TRANSFER-FAILED)
              )
            )
            burn-error (err ERR-TOKEN-TRANSFER-FAILED)
          )
        )
      )
    )
  )
)

;; Emergency functions

(define-public (close-vault)
  (begin
    (asserts! (is-valid-principal tx-sender) (err ERR-INVALID-INPUT))
    (let ((vault (unwrap! (update-vault-interest tx-sender) (err ERR-NO-VAULT))))
      (asserts! (is-eq (get debt vault) u0) (err ERR-NO-DEBT))
      (let ((collateral (get collateral vault)))
        (map-delete vaults tx-sender)
        (if (> collateral u0)
            (match (as-contract (stx-transfer? collateral tx-sender tx-sender))
              success (ok collateral)
              error (err ERR-TOKEN-TRANSFER-FAILED)
            )
            (ok u0)
        )
      )
    )
  )
)

;; Read-only utility functions for debugging and frontend integration

(define-read-only (get-system-info)
  {
    min-collateral-ratio: MIN-COLLATERAL-RATIO,
    liquidation-ratio: LIQUIDATION-RATIO,
    liquidation-bonus: LIQUIDATION-BONUS,
    interest-rate-bp: INTEREST-RATE-BP,
    stx-price: (get-stx-price),
    flux-price: (get-flux-price),
    contract-owner: (get-contract-owner)
  }
)

(define-read-only (calculate-max-borrow (collateral-amount uint))
  (begin
    (asserts! (is-valid-amount collateral-amount) (err ERR-INVALID-INPUT))
    (let ((collateral-value (unwrap! (get-collateral-value collateral-amount) (err ERR-ARITHMETIC-OVERFLOW))))
      (ok (unwrap! (safe-div (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) MIN-COLLATERAL-RATIO) (err ERR-ARITHMETIC-OVERFLOW)))
    )
  )
)

(define-read-only (is-vault-liquidatable (owner principal))
  (begin
    (asserts! (is-valid-principal owner) (err ERR-INVALID-INPUT))
    (match (get-vault owner)
      vault (let (
          (updated-vault (unwrap! (calculate-interest-internal vault) (err ERR-ARITHMETIC-OVERFLOW)))
          (collateral (get collateral updated-vault))
          (debt (get debt updated-vault))
        )
        (if (is-eq debt u0)
          (ok false)
          (let (
              (collateral-value (unwrap! (get-collateral-value collateral) (err ERR-ARITHMETIC-OVERFLOW)))
              (liquidation-threshold (unwrap! (safe-mul debt LIQUIDATION-RATIO) (err ERR-ARITHMETIC-OVERFLOW)))
            )
            (ok (< (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) liquidation-threshold))
          )
        )
      )
      (err ERR-NO-VAULT)
    )
  )
)