;; FluxVerse CDP (Collateralized Debt Position) Contract
;; Allows users to deposit STX as collateral and borrow FLUX tokens
;; Enhanced with governance and partial liquidation features

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

;; New error constants for governance and liquidation
(define-constant ERR-PROPOSAL-NOT-FOUND u112)
(define-constant ERR-PROPOSAL-EXPIRED u113)
(define-constant ERR-ALREADY-VOTED u114)
(define-constant ERR-INSUFFICIENT-VOTING-POWER u115)
(define-constant ERR-PROPOSAL-NOT-PASSED u116)
(define-constant ERR-LIQUIDATION-TOO-LARGE u117)
(define-constant ERR-AUCTION-NOT-ACTIVE u118)
(define-constant ERR-BID-TOO-LOW u119)
(define-constant ERR-AUCTION-EXISTS u120)
(define-constant ERR-INSUFFICIENT-QUORUM u121)
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED u122)

;; System parameters (now variables for governance)
(define-data-var min-collateral-ratio uint u150) ;; 150%
(define-data-var liquidation-ratio uint u130)    ;; 130%
(define-data-var liquidation-bonus uint u10)     ;; 10%
(define-data-var interest-rate-bp uint u500)     ;; 5.00% annualized, in basis points

;; Fixed constants
(define-constant BLOCKS-PER-YEAR u52560)    ;; Assuming ~10 min blocks
(define-constant PRECISION u1000000)        ;; 6 decimal precision for calculations

;; Governance parameters
(define-constant VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant PROPOSAL-THRESHOLD u100000) ;; 100K FLUX tokens to propose
(define-constant QUORUM-THRESHOLD u500000) ;; 500K FLUX tokens for quorum

;; Liquidation parameters
(define-constant MAX-LIQUIDATION-RATIO u50) ;; Max 50% of debt can be liquidated at once
(define-constant AUCTION-DURATION u72) ;; ~12 hours in blocks

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

;; Governance data
(define-data-var proposal-count uint u0)

;; Liquidation auction data
(define-data-var auction-count uint u0)

;; Vault data structure
(define-map vaults principal
  {
    collateral: uint, ;; in micro-STX
    debt: uint,       ;; in FLUX (6 decimals)
    last-block: uint  ;; last interest calculation block
  }
)

;; Governance proposal structure
(define-map proposals uint {
  proposer: principal,
  parameter: (string-ascii 32),
  new-value: uint,
  votes-for: uint,
  votes-against: uint,
  start-block: uint,
  end-block: uint,
  executed: bool
})

;; User votes tracking
(define-map user-votes {proposal-id: uint, voter: principal} {
  amount: uint,
  support: bool
})

;; Liquidation auction structure
(define-map liquidation-auctions uint {
  vault-owner: principal,
  debt-to-cover: uint,
  collateral-for-sale: uint,
  start-block: uint,
  end-block: uint,
  highest-bidder: (optional principal),
  highest-bid: uint,
  is-active: bool
})

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

;; Governance helper functions
(define-private (is-valid-parameter (param (string-ascii 32)) (value uint))
  (or 
    (and (is-eq param "min-collateral-ratio") (and (>= value u110) (<= value u300)))
    (and (is-eq param "liquidation-ratio") (and (>= value u100) (<= value u150)))
    (and (is-eq param "liquidation-bonus") (and (>= value u5) (<= value u20)))
    (and (is-eq param "interest-rate-bp") (and (>= value u0) (<= value u2000)))
  )
)

(define-private (update-parameter (param (string-ascii 32)) (value uint))
  (begin
    (if (is-eq param "min-collateral-ratio")
      (var-set min-collateral-ratio value)
      (if (is-eq param "liquidation-ratio")
        (var-set liquidation-ratio value)
        (if (is-eq param "liquidation-bonus")
          (var-set liquidation-bonus value)
          (if (is-eq param "interest-rate-bp")
            (var-set interest-rate-bp value)
            false
          )
        )
      )
    )
    (ok true)
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
                                (unwrap! (safe-mul debt-value (var-get liquidation-ratio)) (err ERR-ARITHMETIC-OVERFLOW))
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

;; Governance read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (map-get? user-votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-system-parameters)
  {
    min-collateral-ratio: (var-get min-collateral-ratio),
    liquidation-ratio: (var-get liquidation-ratio),
    liquidation-bonus: (var-get liquidation-bonus),
    interest-rate-bp: (var-get interest-rate-bp)
  }
)

;; Liquidation auction read-only functions
(define-read-only (get-auction (auction-id uint))
  (map-get? liquidation-auctions auction-id)
)

(define-read-only (get-current-auction-bonus (auction-id uint))
  (match (map-get? liquidation-auctions auction-id)
    auction (let (
        (elapsed-blocks (- stacks-block-height (get start-block auction)))
        (total-duration AUCTION-DURATION)
        (starting-bonus u20)
        (ending-bonus u5)
        (bonus-decrease (- starting-bonus ending-bonus))
      )
      (if (>= elapsed-blocks total-duration)
        ending-bonus
        (- starting-bonus (/ (* bonus-decrease elapsed-blocks) total-duration))
      ))
    u0
  )
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
                                      (unwrap! (safe-mul current-debt (var-get interest-rate-bp)) (err ERR-ARITHMETIC-OVERFLOW))
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
              (min-collateral-value (unwrap! (safe-mul debt (var-get min-collateral-ratio)) (err ERR-ARITHMETIC-OVERFLOW))))
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

;; Governance functions

(define-public (propose (parameter (string-ascii 32)) (new-value uint))
  (let (
    (proposal-id (+ (var-get proposal-count) u1))
    (proposer-balance (contract-call? FLUX-TOKEN get-balance tx-sender))
  )
    (asserts! (>= proposer-balance PROPOSAL-THRESHOLD) (err ERR-INSUFFICIENT-VOTING-POWER))
    (asserts! (is-valid-parameter parameter new-value) (err ERR-INVALID-INPUT))
    
    (map-set proposals proposal-id {
      proposer: tx-sender,
      parameter: parameter,
      new-value: new-value,
      votes-for: u0,
      votes-against: u0,
      start-block: stacks-block-height,
      end-block: (+ stacks-block-height VOTING-PERIOD),
      executed: false
    })
    
    (var-set proposal-count proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool) (amount uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
    (voter-balance (contract-call? FLUX-TOKEN get-balance tx-sender))
  )
    (asserts! (<= stacks-block-height (get end-block proposal)) (err ERR-PROPOSAL-EXPIRED))
    (asserts! (>= voter-balance amount) (err ERR-INSUFFICIENT-VOTING-POWER))
    (asserts! (is-none (map-get? user-votes {proposal-id: proposal-id, voter: tx-sender})) (err ERR-ALREADY-VOTED))
    
    ;; Record the vote
    (map-set user-votes {proposal-id: proposal-id, voter: tx-sender} {
      amount: amount,
      support: support
    })
    
    ;; Update proposal vote counts
    (if support
      (map-set proposals proposal-id (merge proposal {votes-for: (+ (get votes-for proposal) amount)}))
      (map-set proposals proposal-id (merge proposal {votes-against: (+ (get votes-against proposal) amount)}))
    )
    
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) (err ERR-PROPOSAL-NOT-FOUND)))
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    (votes-for (get votes-for proposal))
    (votes-against (get votes-against proposal))
    (is-executed (get executed proposal))
    (end-block (get end-block proposal))
  )
    ;; Check if voting period has ended
    (asserts! (> stacks-block-height end-block) (err ERR-PROPOSAL-EXPIRED))
    
    ;; Check if proposal hasn't been executed yet
    (asserts! (not is-executed) (err ERR-PROPOSAL-ALREADY-EXECUTED))
    
    ;; Check if proposal passed (more votes for than against)
    (asserts! (> votes-for votes-against) (err ERR-PROPOSAL-NOT-PASSED))
    
    ;; Check if quorum was reached
    (asserts! (>= total-votes QUORUM-THRESHOLD) (err ERR-INSUFFICIENT-QUORUM))
    
    ;; Execute the parameter change
    (unwrap-panic (update-parameter (get parameter proposal) (get new-value proposal)))
    
    ;; Mark as executed
    (map-set proposals proposal-id (merge proposal {executed: true}))
    (ok true)
  )
)

;; Enhanced liquidation functions

(define-public (start-partial-liquidation (target principal) (debt-amount uint))
  (let (
    (vault (unwrap! (update-vault-interest target) (err ERR-NO-VAULT)))
    (max-liquidatable-debt (/ (* (get debt vault) MAX-LIQUIDATION-RATIO) u100))
    (auction-id (+ (var-get auction-count) u1))
  )
    ;; Verify liquidation is needed
    (asserts! (unwrap! (is-vault-liquidatable target) (err ERR-NO-VAULT)) (err ERR-UNDERCOLLATERALIZED))
    
    ;; Ensure we don't liquidate too much
    (asserts! (<= debt-amount max-liquidatable-debt) (err ERR-LIQUIDATION-TOO-LARGE))
    (asserts! (is-none (map-get? liquidation-auctions auction-id)) (err ERR-AUCTION-EXISTS))
    
    ;; Calculate collateral proportional to debt being liquidated
    (let (
      (collateral-ratio (/ (* debt-amount u100) (get debt vault)))
      (collateral-for-sale (/ (* (get collateral vault) collateral-ratio) u100))
    )
      ;; Create auction
      (map-set liquidation-auctions auction-id {
        vault-owner: target,
        debt-to-cover: debt-amount,
        collateral-for-sale: collateral-for-sale,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height AUCTION-DURATION),
        highest-bidder: none,
        highest-bid: u0,
        is-active: true
      })
      
      (var-set auction-count auction-id)
      (ok auction-id)
    )
  )
)

(define-public (bid-on-auction (auction-id uint) (bid-amount uint))
  (let (
    (auction (unwrap! (map-get? liquidation-auctions auction-id) (err ERR-PROPOSAL-NOT-FOUND)))
    (current-bonus (get-current-auction-bonus auction-id))
    (debt-value (get debt-to-cover auction))
    (min-bid (- debt-value (/ (* debt-value current-bonus) u100)))
  )
    (asserts! (get is-active auction) (err ERR-AUCTION-NOT-ACTIVE))
    (asserts! (<= stacks-block-height (get end-block auction)) (err ERR-PROPOSAL-EXPIRED))
    (asserts! (>= bid-amount min-bid) (err ERR-BID-TOO-LOW))
    (asserts! (> bid-amount (get highest-bid auction)) (err ERR-BID-TOO-LOW))
    
    ;; Return previous highest bid if exists
    (match (get highest-bidder auction)
      previous-bidder (try! (contract-call? FLUX-TOKEN transfer (get highest-bid auction) (as-contract tx-sender) previous-bidder none))
      true
    )
    
    ;; Take new bid
    (try! (contract-call? FLUX-TOKEN transfer bid-amount tx-sender (as-contract tx-sender) none))
    
    ;; Update auction
    (map-set liquidation-auctions auction-id (merge auction {
      highest-bidder: (some tx-sender),
      highest-bid: bid-amount
    }))
    
    (ok true)
  )
)

(define-public (finalize-auction (auction-id uint))
  (let (
    (auction (unwrap! (map-get? liquidation-auctions auction-id) (err ERR-PROPOSAL-NOT-FOUND)))
  )
    (asserts! (get is-active auction) (err ERR-AUCTION-NOT-ACTIVE))
    (asserts! (> stacks-block-height (get end-block auction)) (err ERR-PROPOSAL-EXPIRED))
    
    (match (get highest-bidder auction)
      winner (begin
        ;; Burn the FLUX tokens used for bidding
        (try! (as-contract (contract-call? FLUX-TOKEN burn (get highest-bid auction) tx-sender)))
        
        ;; Transfer collateral to winner
        (try! (as-contract (stx-transfer? (get collateral-for-sale auction) tx-sender winner)))
        
        ;; Update vault - reduce debt and collateral
        (let ((vault (unwrap! (get-vault (get vault-owner auction)) (err ERR-NO-VAULT))))
          (map-set vaults (get vault-owner auction) {
            collateral: (- (get collateral vault) (get collateral-for-sale auction)),
            debt: (- (get debt vault) (get debt-to-cover auction)),
            last-block: stacks-block-height
          })
        )
        
        ;; Mark auction as completed
        (map-set liquidation-auctions auction-id (merge auction {is-active: false}))
        (ok true)
      )
      ;; No bidders - extend auction
      (begin
        (map-set liquidation-auctions auction-id (merge auction {
          end-block: (+ stacks-block-height AUCTION-DURATION)
        }))
        (ok false)
      )
    )
  )
)

;; Public functions (existing functionality maintained)

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
      (let ((liquidation-threshold (unwrap! (safe-mul debt (var-get liquidation-ratio)) (err ERR-ARITHMETIC-OVERFLOW))))
        (asserts! (< (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) liquidation-threshold) (err ERR-UNDERCOLLATERALIZED))
        (let (
            (liquidation-bonus-amount (unwrap! (safe-div (unwrap! (safe-mul collateral (var-get liquidation-bonus)) (err ERR-ARITHMETIC-OVERFLOW)) u100) (err ERR-ARITHMETIC-OVERFLOW)))
            (collateral-to-liquidator (unwrap! (safe-add collateral liquidation-bonus-amount) (err ERR-ARITHMETIC-OVERFLOW)))
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
                  bonus: liquidation-bonus-amount
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
    min-collateral-ratio: (var-get min-collateral-ratio),
    liquidation-ratio: (var-get liquidation-ratio),
    liquidation-bonus: (var-get liquidation-bonus),
    interest-rate-bp: (var-get interest-rate-bp),
    stx-price: (get-stx-price),
    flux-price: (get-flux-price),
    contract-owner: (get-contract-owner)
  }
)

(define-read-only (calculate-max-borrow (collateral-amount uint))
  (begin
    (asserts! (is-valid-amount collateral-amount) (err ERR-INVALID-INPUT))
    (let ((collateral-value (unwrap! (get-collateral-value collateral-amount) (err ERR-ARITHMETIC-OVERFLOW))))
      (ok (unwrap! (safe-div (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) (var-get min-collateral-ratio)) (err ERR-ARITHMETIC-OVERFLOW)))
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
              (liquidation-threshold (unwrap! (safe-mul debt (var-get liquidation-ratio)) (err ERR-ARITHMETIC-OVERFLOW)))
            )
            (ok (< (unwrap! (safe-mul collateral-value u100) (err ERR-ARITHMETIC-OVERFLOW)) liquidation-threshold))
          )
        )
      )
      (err ERR-NO-VAULT)
    )
  )
)
